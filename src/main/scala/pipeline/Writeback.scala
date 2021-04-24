package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import utils.Config._
import utils.Fixed._
import pipeline.RegisterFileType._

class WritebackIO extends Bundle {
  val in = Flipped(new ExWbIO)
  val out = new WbIdIO
  val fwd = new WbFwdIO
}

object WritebackState extends ChiselEnum {
  val sBuild, sOutput = Value
}

class Writeback extends Module {
  require(SUBVECTORS_PER_VREG > 2, "Writeback stage currently does not support subvectors_per_vreg <= 2")
  import WritebackState._
  val io = IO(new WritebackIO)
  val in = RegNext(io.in)

  /** Buffer for building results to be written back */
  val writeBuffer = RegInit(VecInit(Seq.fill(SUBVECTORS_PER_VREG-1)(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))))
  /** State register for output FSM */
  val state = RegInit(sOutput)
  /** Pointer to writeable location in writeBuffer */
  val ptr = RegInit(0.U(log2Ceil(SUBVECTORS_PER_VREG+1).W))
  /** Result being output */
  val res = VecInit(Seq.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W)))
  /** Write enable flag */
  val we = WireDefault(false.B)

  //This state machine assumes that SUBVECTORS_PER_VREG > 2. Should probably also work for SUBVECTORS_PER_VREG <= 2
  switch(state) {
    is(sBuild) {
      when(in.valid) {
        writeBuffer(ptr) := in.res
        ptr := ptr + 1.U
      }

      when(in.valid && ptr === (SUBVECTORS_PER_VREG-2).U) {
        state := sOutput
        ptr := ptr + 1.U
      }
    }

    is(sOutput) {
      //Valid, start building vreg output
      when(in.valid && in.dest.rf === VREG && ptr === 0.U) {
        writeBuffer(ptr) := in.res
        ptr := ptr + 1.U
        state := sBuild
      }

      //Valid, push out vreg output
      when(in.valid && in.dest.rf === VREG && ptr === (SUBVECTORS_PER_VREG-1).U) {
        //Output is a mix of values in writeBuffer and recently arrived result
        for(i <- 0 until VREG_DEPTH-NUM_PROCELEM) {
          val wb = i/NUM_PROCELEM
          val j = i % NUM_PROCELEM
          res(i) := writeBuffer(wb)(j)
        }
        for(i <- VREG_DEPTH-NUM_PROCELEM until VREG_DEPTH) {
          res(i) := in.res(i-(VREG_DEPTH-NUM_PROCELEM))
        }
        we := true.B
        ptr := 0.U
      }

      //Valid, single-cycle xreg or sreg output
      when(in.valid && (in.dest.rf === XREG || (in.dest.rf === SREG && !in.reduce))) {
        for(i <- 0 until NUM_PROCELEM) {
          res(i) := in.res(i)
        }
        for(i <- NUM_PROCELEM until VREG_DEPTH) {
          res(i) := 0.S(FIXED_WIDTH.W)
        }
        we := true.B
      }

      when(in.valid && (in.dest.rf === SREG && in.reduce)) {
        for(i <- 0 until NUM_PROCELEM) {
          res(i) := in.res.reduceTree( (a,b) => a+b )
        }
        for(i <- NUM_PROCELEM until VREG_DEPTH) {
          res(i) := 0.S(FIXED_WIDTH.W)
        }
        we := true.B
      }
    }
  }
  io.out.wrData := res
  io.out.rd := in.dest
  io.out.we := we

  //Assign outputs to forwarding stage
  //Position '0' corresponds to ptr=0, position '1' to ptr=1 and final position corresponds to most value on pipeline register
  for(i <- 0 until writeBuffer.length) {
    io.fwd.wbData(i) := writeBuffer(i)
    io.fwd.rdValids(i) := ptr > i.U
    io.fwd.rd(i).reg := in.dest.reg
    io.fwd.rd(i).rf := in.dest.rf
    io.fwd.rd(i).rfUint := in.dest.rfUint
    io.fwd.rd(i).subvec := i.U
  }
  io.fwd.wbData(io.fwd.wbData.length-1) := in.res
  io.fwd.rdValids(io.fwd.rdValids.length-1) := in.valid
  io.fwd.rd(io.fwd.rd.length-1) := in.dest
}

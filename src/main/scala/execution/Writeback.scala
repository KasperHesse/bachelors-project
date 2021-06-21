package execution

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import utils.Config._
import utils.Fixed._
import execution.RegisterFileType._

class WritebackIO extends Bundle {
  val ex = Flipped(new ExWbIO)
  val id = new WbIdIO
  val fwd = new WbFwdIO
}

object WritebackState extends ChiselEnum {
  val sBuildVreg, sBuildXreg, sOutput = Value
}

/**
 * The writeback module in the execute stage. This module receives outputs from the execute stage and holds them until an entire result has arrived,
 * then writes that result in the correct register file.
 */
class Writeback extends Module {
  require(SUBVECTORS_PER_VREG > 2, "Writeback stage currently does not support SUBVECTORS_PER_VREG <= 2")
  import WritebackState._
  val io = IO(new WritebackIO)
  val in = RegNext(io.ex)

  /** Method to generate an adder tree for reducing results */
  def adderTreeFunc(in: Vec[SInt]): SInt = {
    in.reduceTree((a,b) => a+b)
  }
  /** Buffer for building results to be written back */
  val writeBuffer = RegInit(VecInit(Seq.fill(SUBVECTORS_PER_VREG-1)(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))))
  /** State register for output FSM */
  val state = RegInit(sOutput)
  /** Pointer to writeable location in writeBuffer */
  val cnt = RegInit(0.U(log2Ceil(SUBVECTORS_PER_VREG+1).W))
  /** Result being output */
  val res = VecInit(Seq.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W)))
  /** Write enable flag */
  val we = WireDefault(false.B)

  val adderTree = adderTreeFunc(in.res)

  //This state machine assumes that SUBVECTORS_PER_VREG > 2. Should probably also work for SUBVECTORS_PER_VREG <= 2
  //Next state logic
  switch(state) {
    is(sBuildVreg) {
      when(in.valid) {
        writeBuffer(cnt) := in.res
        cnt := cnt + 1.U
      }
      when(in.valid && cnt === (SUBVECTORS_PER_VREG-2).U) {
        state := sOutput
        cnt := cnt + 1.U
      }
    }
    is(sBuildXreg) {
      when(in.valid) {
        writeBuffer(0)(cnt) := adderTree
        cnt := cnt + 1.U
      }
      when(in.valid && cnt === (XREG_DEPTH-2).U) {
        state := sOutput
        cnt := cnt + 1.U
      }
    }

    is(sOutput) {
      //Valid, start building vreg output
      when(in.valid && in.dest.rf === VREG && cnt === 0.U) {
        writeBuffer(cnt) := in.res
        cnt := cnt + 1.U
        state := sBuildVreg
      }
      //Valid, start building xreg output
      when(in.valid && in.dest.rf === XREG && cnt === 0.U && in.reduce) {
        writeBuffer(0)(cnt) := adderTree
        cnt := cnt + 1.U
        state := sBuildXreg
      }
    }
  }

  //Output logic
  switch(state) {
    is(sOutput) {
      //Valid, push out vreg output
      when(in.valid && in.dest.rf === VREG && cnt === (SUBVECTORS_PER_VREG-1).U) {
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
        cnt := 0.U
      }

      //Valid, single-cycle xreg or sreg output
      when(in.valid && (in.dest.rf === XREG || in.dest.rf === SREG) && !in.reduce) {
        for(i <- 0 until NUM_PROCELEM) {
          res(i) := in.res(i)
        }
        we := true.B
      }

      //valid, reduced output from red.vv
      when(in.valid && in.dest.rf === XREG && in.reduce && cnt === (XREG_DEPTH-1).U) {
        for(i <- 0 until XREG_DEPTH-1) {
          res(i) := writeBuffer(0)(i)
          res(XREG_DEPTH-1) := adderTree
        }
        we := true.B
        cnt := 0.U
      }

      //Valid, reduced result from mac.sv, mac.vv or red.xx
      when(in.valid && in.dest.rf === SREG && in.reduce) {
        res(0) := adderTree //and nothing more than that
        we := true.B
      }
    }
  }
  io.id.wrData := res
  io.id.rd := in.dest
  io.id.we := we



  //Assign outputs to forwarding stage
  //Position '0' corresponds to cnt=0, position '1' to cnt=1 and final position corresponds to most recent value on pipeline register
  for(i <- 0 until writeBuffer.length) {
    io.fwd.wbData(i) := writeBuffer(i)
    io.fwd.rdValids(i) := cnt > i.U
    io.fwd.rd(i).reg := in.dest.reg
    io.fwd.rd(i).rf := in.dest.rf
    io.fwd.rd(i).rfUint := in.dest.rfUint
    io.fwd.rd(i).subvec := i.U
  }
  io.fwd.wbData(io.fwd.wbData.length-1) := in.res
  io.fwd.rdValids(io.fwd.rdValids.length-1) := in.valid
  io.fwd.rd(io.fwd.rd.length-1) := in.dest
}
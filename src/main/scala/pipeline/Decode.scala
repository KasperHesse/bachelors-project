package pipeline

import chisel3._
import chisel3.util._
import utils.Config._
import utils.Fixed._
import DecodeStage._
import vector.KEWrapper
import chisel3.experimental.BundleLiterals._
import chisel3.experimental.ChiselEnum
import vector.Opcode._

class Decode extends Module {
  val io = IO(new DecodeIO)

  // --- CONSTANTS ---
  private val vregSlotWidth: Int = NUM_VECTOR_REGISTERS/NUM_VREG_SLOTS
  private val numSubVectors: Int = VECTOR_REGISTER_DEPTH/NUM_PROCELEM

  // --- MODULES ---
  val regFile = Module(new VectorRegisterFile(NUM_VECTOR_REGISTERS, VECTOR_REGISTER_DEPTH, VECTOR_REGISTER_DEPTH))

  // --- REGISTERS AND WIRES ---
  /** Instruction buffer */
  val iBuffer = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  /** Instruction pointer into the instruction buffer */
  val IP = RegInit(0.U(4.W))
  /** Number of instruction in the current instruction buffer */
  val iCount = RegInit(0.U(4.W))
  /** Current state for the decoder FSM */
  val stateReg = RegInit(sIdle)
  /** Used to index into subvectors */
  val X = RegInit(0.U(log2Ceil(VECTOR_REGISTER_DEPTH).W))
  /** Used to select which vector from a vector slot is output */
  val slotSelect = RegInit(0.U(log2Ceil(vregSlotWidth).W))
  /** Pipeline register holding all input values. Only loads when io.ctrl.iload is asserted */
  val in = RegEnable(io.in, io.ctrl.iload) //We only load in values when we're ready and not being stalled by the control unit

  val iload = RegNext(io.ctrl.iload)
  /** Shortcut to access the current instruction */
  val currentInstr = iBuffer(IP)

  val Rinst = currentInstr.asTypeOf(new RtypeInstruction)
  val slot1 = Rinst.rs1
  val slot2 = Rinst.rs2

  //Generate subvectors for selection
  //This makes it easier to address subvectors of the output vectors a, b
  val a_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until numSubVectors) {
    a_subvec(i) := regFile.io.rdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := regFile.io.rdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }
  /** Currently adressed 'a' vector going into execute stage */
  val a = a_subvec(X)
  /** Currently adressed 'b' vector going to execute stage */
  val b = b_subvec(X)

  //Generate vector register selects based on slot defined in instruction + slot select
  val rs1 = (slot1 << log2Ceil(vregSlotWidth)).asUInt + slotSelect
  val rs2 = (slot2 << log2Ceil(vregSlotWidth)).asUInt + slotSelect

  //Register updates
  val Xtick: Bool = X === (numSubVectors - 1).U
  val SStick: Bool = (slotSelect === (vregSlotWidth-1).U)
  val IPtick: Bool = (IP === iCount)
  val finalCycle: Bool = Xtick && SStick && IPtick

  // --- DEFAULT CONNECTIONS ---
  //Register file inputs
  regFile.io.rs1 := rs1
  regFile.io.rs2 := rs2

  //Output connections
  io.out.a := a
  io.out.b := b
  io.out.dest.rd := Rinst.rd
  io.out.dest.subvec := X
  io.out.op := Rinst.op
  io.ctrl.state := stateReg
  io.ctrl.finalCycle := finalCycle

  //Dontcares
  regFile.io.wrData := DontCare
  regFile.io.we := false.B
  regFile.io.rd := 0.U
  regFile.io.wrMask := 0.U
  regFile.io.rdMask2 := X
  regFile.io.rdMask1 := X
  io.out.macLimit := 0.U
  // --- LOGIC ---

  //From idle state, move to "load" state when a vstart/estart command is asserted
  //Keep doing this until control signal "iload" goes low. Then move to execute stage
  switch(stateReg) {
    is(sIdle) {
      io.out.op := NOP
      val instr = in.instr.asTypeOf(new OtypeInstruction)
      when((instr.iev === OtypeIEV.ELEM || instr.iev === OtypeIEV.VEC) && instr.se === OtypeSE.START && io.ctrl.iload) {
        stateReg := sLoad
      }
    }

    is(sLoad) {
      iBuffer(IP) := in.instr
      IP := IP + 1.U
      when(!io.ctrl.iload) {
        stateReg := sExecute
        IP := 0.U
        iCount := IP
        //Reset registers for tracking progress
        X := 0.U
        slotSelect := 0.U
      }
    }

    is (sExecute) {
      X := Mux(Xtick, 0.U, X + 1.U)
      slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
      IP := Mux(SStick && Xtick, IP + 1.U, IP)

      //TODO Optimize this. We should only be stalled if the upcoming operation is of a different type than the currently
      //executing op
      when(X === 0.U && slotSelect === 0.U && io.ctrl.exproc) {
        X := X
        slotSelect := slotSelect
        IP := IP
        io.out.op := NOP
      }

      //We need to check if the execute stage is still processing. If this is the case, we will maintain all values until deasserted

      when(finalCycle) {
        IP := 0.U
        when(io.ctrl.iload) {
          stateReg := sLoad
        } .otherwise {
          stateReg := sIdle
        }

      }
    }
  }
}

class DecodeIO extends Bundle {
  val out = new IdExIO
  val in = Flipped(new IpIdIO)
  val ctrl = new IdControlIO
}

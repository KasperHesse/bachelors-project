package pipeline

import chisel3._
import chisel3.util._
import utils.Config._
import utils.Fixed._
import DecodeOldStage._
import vector.{KEWrapper, Opcode}
import chisel3.experimental.BundleLiterals._
import vector.Opcode._
import RegisterFileType._

class DecodeOld extends Module {
  val io = IO(new DecodeOldIO)

  // --- CONSTANTS ---
  private val numSubVectors: Int = SUBVECTORS_PER_VREG

  // --- MODULES ---
  /** Vector register file. Has [[NUM_VREG]] entries, each of which holds [[VREG_DEPTH]] elements */
  val vRegFile = Module(new VectorRegisterFile(NUM_VREG, VREG_DEPTH, VREG_DEPTH))
  /** X-value vector register file. Has [[NUM_VREG]]/[[NUM_VREG_SLOTS]] entries, each of which holds [[VREG_SLOT_WIDTH]] values */
  val xRegFile = Module(new VectorRegisterFile(NUM_XREG, VREG_SLOT_WIDTH, VREG_SLOT_WIDTH))
  /** Scalar register file. Has [[NUM_SREG]] entries */
  val sRegFile = Module(new ScalarRegisterFile)
  /** Wrapper for KE-matrix, holding all KE-values */
  val KE = Module(new KEWrapper(NUM_PROCELEM, sync=false, SIMULATION))

  // --- REGISTERS ---
  /** Instruction buffer */
  val iBuffer = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  /** Instruction pointer into the instruction buffer */
  val IP = RegInit(0.U(4.W))
  /** Number of instruction in the current instruction buffer */
  val iCount = RegInit(0.U(4.W))
  /** Current state for the instruction load/execution FSM */
  val iloadStateReg = RegInit(sIdle)
  /** Current index into subvectors. Also gives the x-coordinate of the submatrix in the KE matrix */
  val X = RegInit(0.U(log2Ceil(VREG_DEPTH).W))
  /** Current y-coordinate used to index into KE matrix */
  val Y = RegInit(0.U(log2Ceil(KE_SIZE/NUM_PROCELEM).W))
  /** Current column of submatrix (x,y) in the KE matrix */
  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM).W))
  /** Used to select which vector from a vector slot is output */
  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH).W))


  /** Pipeline register holding all input values. Only loads when io.ctrl.iload is asserted */
  val in = RegNext(io.in)
  /** Shortcut to access the current instruction */
  val currentInstr = iBuffer(IP)

  // --- WIRES ---

  val Rinst = currentInstr.asTypeOf(new RtypeInstruction)
  val Sinst = currentInstr.asTypeOf(new StypeInstruction)
  val slot1 = Rinst.rs1 //Vector slot accessed by rs1
  val slot2 = Rinst.rs2 //Vector slot accessed by rs2

  /** Specifies that the system is in a state where values should be output (aka processing an Rtype instruction) */
  val outputState: Bool = iloadStateReg === sExecute && (Rinst.fmt === InstructionFMT.RTYPE)
  /** True when IP has reached its max-value */
  val IPtick: Bool = (IP === iCount)
  /** Indicates that the final clock cycle of the current instruction is being executed */
  val finalCycle: Bool = WireDefault(false.B)


  val firstCycle = (X === 0.U) && (Y === 0.U) && (col === 0.U) && (slotSelect === 0.U)

  //Generate subvectors for selection
  //This makes it easier to address subvectors of the output vectors a, b from register file
  val a_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until numSubVectors) {
    a_subvec(i) := vRegFile.io.rdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := vRegFile.io.rdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }
  /** 'a' data subvector going into execute stage */
  val a = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
  /** 'b' data subvector going to execute stage */
  val b = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
  /** Opcode going into execution stage */
  val op = WireDefault(NOP)
  /** Destination register for current operation */
  val dest = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.subvec -> 0.U, _.rf -> VREG))
  /** Limit for MAC operations, if such a one is being processed */
  val macLimit = WireDefault(0.U(32.W))
  /** Signals that the outgoing operation should be added to the destination queue */
  val newDest = WireDefault(false.B)

  //Generate vector register selects based on slot defined in instruction + slot select
  val v_rs1 = (slot1 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  val v_rs2 = (slot2 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  val v_rd = (Rinst.rd << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect

  // --- DEFAULT CONNECTIONS & DontCares ---
  //Register file inputs
  vRegFile.io.rs1 := v_rs1
  vRegFile.io.rs2 := v_rs2

  xRegFile.io.rs1 := slot1
  xRegFile.io.rs2 := slot2

  sRegFile.io.rs1 := slot1
  sRegFile.io.rs2 := slot2

  //KE connections
  KE.io.keX := X
  KE.io.keY := Y
  KE.io.keCol := col

  //Dontcares
  vRegFile.io.wrData := DontCare
  vRegFile.io.we := false.B
  vRegFile.io.rd := 0.U
  vRegFile.io.wrMask := 0.U
  vRegFile.io.rdMask2 := X
  vRegFile.io.rdMask1 := X

  xRegFile.io.we := false.B
  xRegFile.io.rd := 0.U
  xRegFile.io.wrData := DontCare
  xRegFile.io.wrMask := 0.U
  xRegFile.io.rdMask1 := X
  xRegFile.io.rdMask2 := X

  sRegFile.io.wrData := DontCare
  sRegFile.io.rd := 0.U
  sRegFile.io.we := 0.U

  //Output connections
  io.ex.a := a
  io.ex.b := b
  io.ex.dest:= dest
  io.ex.op := op
  io.ex.macLimit := macLimit
  io.ex.newDest := newDest
  io.ctrl.rtypemod := Rinst.mod
  io.ctrl.state := iloadStateReg
  io.ctrl.finalCycle := finalCycle
  io.ctrl.firstCycle := firstCycle
  io.ctrl.op := op


  // --- LOGIC ---

  //Ip update
  IP := Mux(finalCycle, IP + 1.U, IP)

  //Output selection logic
  when(outputState) {
    switch(Rinst.mod) {
      is(RtypeMod.VV) { //This won't work when processing SUM instructions. They are special cases
        //Output connections
        a := a_subvec(X)
        b := b_subvec(X)
        op := Rinst.op
        dest.reg := v_rd
        dest.subvec := X
        dest.rf := VREG
        //TODO This is not the correct way of setting these values. MacLImit should be much higher (nelem or ndof)
        macLimit := Mux(Rinst.op === MAC, (NUM_VREG_SLOTS*VREG_DEPTH/NUM_PROCELEM).U, 0.U)

        newDest := Mux(Rinst.op === MAC, slotSelect === 0.U && X === 0.U, Mux(Rinst.op === MAC, false.B, true.B))

        //Updates
        val Xtick = X === (numSubVectors - 1).U
        val SStick: Bool = (slotSelect === (VREG_SLOT_WIDTH-1).U)

        X := Mux( Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      is(RtypeMod.XV) {
        //XV instructions take the first element in 'x' and operate with all elements in the first vector of 'v'
        //SlotSelect is used to index into xReg, X is used to index into vReg
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := xRegFile.io.rdData1(slotSelect)
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.reg := v_rd
        dest.subvec := X
        dest.rf := VREG
        newDest := true.B

        //Updates
        val Xtick = (X === (numSubVectors - 1).U)
        val SStick: Bool = (slotSelect === (VREG_SLOT_WIDTH-1).U)

        X := Mux(Xtick, 0.U, X+1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      is(RtypeMod.XX) {
        //Operates in the same way as VV decode, except it only takes one clock cycle
        a := xRegFile.io.rdData1
        b := xRegFile.io.rdData2
        op := Rinst.op
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := XREG
        newDest := true.B

//        X := Mux(X === 1.U, 0.U, X + 1.U)
//        when(X =/= 0.U) {
//          op := NOP
//        }
//        finalCycle := X === 1.U
        finalCycle := true.B
      }

      is(RtypeMod.SV) {
        //Output connections
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := sRegFile.io.rdData1
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.reg := v_rd
        dest.subvec := X
        dest.rf := VREG
        newDest := true.B
        //Updates
        val Xtick = X === (numSubVectors - 1).U
        val SStick: Bool = (slotSelect === (VREG_SLOT_WIDTH-1).U)

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      //TODO do we also want SX instructions?
      //If we reduce the register size, we can increase the MOD field to 4 bits
      //Might also make for easier decode logic
      //Right operand determines how x,y,col values are updated, left operand simply follows and outputs

      is(RtypeMod.SS) {
        //Operates just like XX decode, except the same value is used on all ports
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := sRegFile.io.rdData1
          b(i) := sRegFile.io.rdData2
        }
          op := Rinst.op
          dest.reg := Rinst.rd
          dest.subvec := 0.U
          dest.rf := SREG
          newDest := true.B
          finalCycle := true.B
      }

      is(RtypeMod.KV) {
        a := KE.io.keVals
        for(i <- 0 until NUM_PROCELEM) {
          b(i) := a_subvec(X)(col) //Notice: We're using a_subvec since we're fetching via rs1 and not rs2
        }
        op := MAC
        macLimit := KE_SIZE.U
        dest.reg := v_rd
        dest.subvec := Y
        newDest := (X === 0.U) && (col === 0.U)

        val colTick = (col === (NUM_PROCELEM-1).U)
        val Xtick = (X === (KE_SIZE/NUM_PROCELEM-1).U)
        val Ytick = (Y === (KE_SIZE/NUM_PROCELEM-1).U)
        val SStick = (slotSelect === (VREG_SLOT_WIDTH-1).U)
        //Updates
        col := Mux(colTick, 0.U, col + 1.U)
        X := Mux(colTick, Mux(Xtick, 0.U, X + 1.U), X)
        Y := Mux(Xtick && colTick, Mux(Ytick, 0.U, Y + 1.U), Y)
        slotSelect := Mux(Xtick && Ytick && colTick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)

        finalCycle := Xtick && Ytick && colTick && SStick
      }
    }
  }


  //Instruction load/execute FSM
  //TODO change this entire FSM to reflect the new structure where instructions are loaded outside
  //Most of this can be placed in the Decode-module. The remainder of a lot of this must be moved into "Thread" modules
  switch(iloadStateReg) {
    is(sIdle) {
      op := NOP
      val instr = in.instr.asTypeOf(new OtypeInstruction)
      //TODO Change this to
      when((instr.iev === OtypeIEV.EXEC || instr.iev === OtypeIEV.VEC) && instr.se === OtypeSE.START && io.ctrl.iload) {
        iloadStateReg := sLoad
      }
    }

    is(sLoad) {
      X := 0.U
      slotSelect := 0.U
      Y := 0.U
      col := 0.U
      op := NOP
      iBuffer(IP) := in.instr
      IP := IP + 1.U

      when(!io.ctrl.iload) {
        iloadStateReg := sExecute
        IP := 0.U
        iCount := IP
      }
    }

    is (sExecute) {
      //Finished processing all operations
      when(finalCycle && IPtick) {
        IP := 0.U
        when(io.ctrl.iload) {
          iloadStateReg := sLoad
        } .otherwise {
          iloadStateReg := sIdle
        }
      }
    }
  }

  //Stall management
  when(io.ctrl.stall) {
    X := X
    slotSelect := slotSelect
    Y := Y
    col := col
    IP := IP
    newDest := false.B
    iloadStateReg := iloadStateReg
  }
}
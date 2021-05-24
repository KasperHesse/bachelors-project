package pipeline

import chisel3._
import ThreadState._
import chisel3.util._
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import vector.KEWrapper
import vector.Opcode._
import chisel3.experimental.BundleLiterals._
import memory.IJKBundle
import pipeline.RegisterFileType._

/**
 * I/O Ports for "Thread" modules inside of the decode stage
 */
class ThreadIO extends Bundle {
  /** `i` value in the current grid, used to index into memory elemeents */
  val i = Input(UInt(log2Ceil(NELX+1).W))
  /** `j` value in the current grid, used to index into memory elements */
  val j = Input(UInt(log2Ceil(NELY+1).W))
  /** `k` value in the current grid, used to index into memory elements */
  val k = Input(UInt(log2Ceil(NELZ+1).W))
  /** How far into the current vector we have progressed. Used when loading/storing with .vec suffix */
  val progress = Input(UInt(log2Ceil(NDOF+1).W))
  /** The current instruction, fetched from instruction buffer in decode stage */
  val instr = Input(UInt(INSTRUCTION_WIDTH.W))
  /** Finished flag. Asserted when processing is finished and the thread should move to idle state after writing back */
  val fin = Input(Bool())
  /** Start flag. Asserted when a packet has been transferred from IM to instructionBuffer, and processing may begin */
  val start = Input(Bool())
  /** Instruction pointer of the current packet. Sent to IM in Decode stage */
  val ip = Output(UInt(4.W))
  /** Connections to other thread in Decode stage */
  val thread = new ThreadThreadIO
  /** The literal value of the state. Only used for debugging purposes since we cannot peek enums yet */
  val stateOutUint = Output(UInt(8.W))
  /** Connections to the execute stage */
  val ex = new IdExIO
  /** Connections to memory stage */
  val mem = Decoupled(new IdMemIO)
  /** Connections to the control module */
  val ctrl = new ThreadControlIO
  /** Connections to writeback stage */
  val wb = Flipped(new WbIdIO)
  /** Connections to shared register file in Decode stage */
  val sRegFile = Flipped(new ScalarRegFileIO)
}

class ThreadThreadIO extends Bundle {
  /** Current state of this thread */
  val stateIn = Input(ThreadState())
  /** Current state of the other thread */
  val stateOut = Output(ThreadState())

//  val swapIn = Input(Bool())
//  val swapOut = Output(Bool())
//  val ijkOut = Output(new IJK)
//  val ijkIn = Input(new IJK)
}

/**
 * A module representing a thread in the decode stage. Each decode stage contains two threads. Implements [[ThreadIO]]
 * @param id The ID of the thread. If id=0, this thread will be the first to fetch data from memory.
 *           If ID=1, this thread will wait until thread 0 has started executing, at which point this one will start loading
 */
class Thread(id: Int) extends Module {
  require(id == 0 || id == 1, "Thread ID must be 0 or 1")
  val io = IO(new ThreadIO)

  // --- MODULES ---
  /** Vector register file. Has [[utils.Config.NUM_VREG]] entries, each of which holds [[utils.Config.VREG_DEPTH]] elements */
  val vRegFile = Module(new VectorRegisterFile(NUM_VREG, VREG_DEPTH, VREG_DEPTH))
  /** X-value vector register file. Has [[utils.Config.NUM_XREG]] entries, each of which holds [[utils.Config.VREG_SLOT_WIDTH]] values */
  val xRegFile = Module(new VectorRegisterFile(NUM_XREG, VREG_SLOT_WIDTH, VREG_SLOT_WIDTH))
  /** Wrapper for KE-matrix, holding all KE-values */
  val KE = Module(new KEWrapper(NUM_PROCELEM, sync=false, SIMULATION))
  /** Immediate generator */
  val immGen = Module(new ImmediateGenerator)

  // --- REGISTERS ---
  /** Instruction pointer into the instruction buffer */
  val IP = RegInit(0.U(4.W))
  /** Current index into subvectors. Also gives the x-coordinate of the submatrix in the KE matrix */
//  val X = RegInit(0.U(log2Ceil(VREG_DEPTH).W))
  val X = RegInit(0.U)
  /** Current y-coordinate used to index into KE matrix */
//  val Y = RegInit(0.U(log2Ceil(KE_SIZE/NUM_PROCELEM).W))
  val Y = RegInit(0.U)
  /** Current column of submatrix (x,y) in the KE matrix */
//  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM).W))
  val col = RegInit(0.U)
  /** Used to select which vector from a vector slot is output */
//  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH).W))
  val slotSelect = RegInit(0.U)
  /** State register */
  val state = RegInit(sIdle)

  // --- WIRES AND SIGNALS ---
  /** Handle to the current instruction */
  val currentInstr: UInt = io.instr
  /** Length of the instruction being decoded */
  val instrLen = RegInit(1.U(log2Ceil(NDOFLENGTH+1).W))
  /** LUT to decode vector lengths (for setting macLimit) */
  val lenDecode = VecInit(Array(
    (NDOFLENGTH/NUM_PROCELEM).U,
    (NELEMLENGTH/NUM_PROCELEM).U,
    (ELEMS_PER_VSLOT/NUM_PROCELEM).U //allows us to perform a single mac instruction on stored values
  ))

  val Rinst = currentInstr.asTypeOf(new RtypeInstruction)
  val Oinst = currentInstr.asTypeOf(new OtypeInstruction)
  val Sinst = currentInstr.asTypeOf(new StypeInstruction)

  /** VREG slot as specified by rs1 */
  val slot1 = Rinst.rs1
  /** VREG slot as specified by rs2 */
  val slot2 = Rinst.rs2
  /** Asserted whenever the first cycle of an instruction is being processed. Used by the control unit */
  val firstCycle = IP =/= RegNext(IP)
  /** Used to indicate whenever an instruction is finished and the IP should update */
  val finalCycle = WireDefault(false.B)

  //This makes it easier to address subvectors of the output vectors a, b from register file
  val a_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until SUBVECTORS_PER_VREG) {
    a_subvec(i) := vRegFile.io.rdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := vRegFile.io.rdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }

  /** 'a' data subvector going into execute stage */
  val a = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W)))) //TODO Change default assignment to a_subvec(0)
  /** 'b' data subvector going to execute stage */
  val b = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
  /** Bundle holding the values necessary to identify register source 1 */
  val rs1 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
  /** Bundle holding the values necessary to identify register source 2 */
  val rs2 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
  /** Opcode going into execution stage */
  val op = WireDefault(NOP)
  /** Destination register for current operation */
  val dest = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.subvec -> 0.U, _.rf -> VREG))
  /** Limit for MAC operations, if such a one is being processed */
  val macLimit = WireDefault(0.U(32.W))
  /** Signals that the outgoing operation should be added to the destination queue */
  val valid = WireDefault(false.B)


  //Declare variables here to allow assignment in if-statement
  /** Index into vector register file */
  val v_rs1 = Wire(UInt(log2Ceil(NUM_VREG+1).W))
  /** Index into vector register file */
  val v_rs2 = Wire(UInt(log2Ceil(NUM_VREG+1).W))
  /** Index into vector register file */
  val v_rd = Wire(UInt(log2Ceil(NUM_VREG+1).W))
  //Generate vector register selects based on slot defined in instruction + slot select
  //Use lookup tables to avoid multiplications in case we don't have a power of two
  if(!isPow2(VREG_SLOT_WIDTH)) {
    val vRegLookup = Wire(Vec(3, Vec(NUM_VREG_SLOTS, UInt(log2Ceil(NUM_VREG+1).W))))
    for(i <- 0 until NUM_VREG_SLOTS) {
      vRegLookup(0)(i) := (i*VREG_SLOT_WIDTH).U
      vRegLookup(1)(i) := (i*VREG_SLOT_WIDTH).U
      vRegLookup(2)(i) := (i*VREG_SLOT_WIDTH).U
    }
    v_rs1 := vRegLookup(0)(slot1) + slotSelect
    v_rs2 := vRegLookup(1)(slot2) + slotSelect
    v_rd := vRegLookup(2)(Rinst.rd) + slotSelect
  } else {
    v_rs1 := (slot1 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
    v_rs2 := (slot2 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
    v_rd := (Rinst.rd << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  }

  // --- OUTPUT & MODULE CONNECTIONS ---
  //Register file and KE connections
  vRegFile.io.rs1 := v_rs1
  vRegFile.io.rs2 := v_rs2
  vRegFile.io.wrData := io.wb.wrData
  vRegFile.io.we := io.wb.we && (io.wb.rd.rf === RegisterFileType.VREG)
  vRegFile.io.rd := io.wb.rd.reg

  xRegFile.io.rs1 := Rinst.rs1
  xRegFile.io.rs2 := Rinst.rs2
  xRegFile.io.wrData := io.wb.wrData.slice(0, XREG_DEPTH)
  xRegFile.io.we := io.wb.we && (io.wb.rd.rf === RegisterFileType.XREG)
  xRegFile.io.rd := io.wb.rd.reg

  io.sRegFile.rs1 := Rinst.rs1
  io.sRegFile.rs2 := Rinst.rs2
  KE.io.keX := X
  KE.io.keY := Y
  KE.io.keCol := col

  immGen.io.instr := Rinst
  io.ex.imm := immGen.io.imm
  io.ex.useImm := Rinst.immflag

  //Dontcares
  vRegFile.io.wrMask := 0.U
  vRegFile.io.rdMask2 := X
  vRegFile.io.rdMask1 := X

  xRegFile.io.wrMask := 0.U
  xRegFile.io.rdMask1 := X
  xRegFile.io.rdMask2 := X

  //All inputs to scalar reg file are supplied via writeback stage, hence these are dontcares
  io.sRegFile.we := DontCare
  io.sRegFile.rd := DontCare
  io.sRegFile.wrData := DontCare

  //Outputs
  io.ex.a := a
  io.ex.b := b
  io.ex.dest:= dest
  io.ex.op := op

  io.ex.macLimit := macLimit
  io.ex.valid := valid
  io.ex.firstCycle := firstCycle
  io.ex.rs1 := rs1
  io.ex.rs2 := rs2
  io.ctrl.rtypemod := Rinst.mod
  io.ctrl.state := state
  io.ctrl.finalCycle := finalCycle
  io.ctrl.firstCycle := firstCycle
  io.ctrl.op := op
  io.ctrl.rs1 := rs1
  io.ctrl.rs2 := rs2
  io.thread.stateOut := state
  io.ip := IP

  /** Debug values */
  //TODO Remove these
  io.stateOutUint := state.asUInt()
  io.ctrl.stateUint := state.asUInt()
  io.ex.dest.rfUint := dest.rf.asUInt()
  io.ex.opUInt := op.asUInt()
  io.mem := DontCare


  // --- LOGIC ---
  //IP update logic, default. Overridden by assignments in next state logic
  IP := Mux(finalCycle, IP + 1.U, IP)

  //Next state logic
  switch(state) {
    is(sIdle) {
      IP := 0.U
      when(Oinst.pe === OtypePE.PACKET && Oinst.se === OtypeSE.START && Oinst.fmt === InstructionFMT.OTYPE && io.start) {
        IP := 1.U
        instrLen := lenDecode(Oinst.len.asUInt())
        if(id == 0) {
          state := sLoad
        } else {
          state := sWait1
        }
      }
    }
    is(sLoad) {
      //Load data for this instruction, increment pointer as necessary
      when(Oinst.pe === OtypePE.EXEC && Oinst.se === OtypeSE.START) {
        finalCycle := false.B //This is funky, but required for correct functionality
        state := sEstart
      }
    }
    is(sEstart) {
      when(io.thread.stateIn === sEend || io.thread.stateIn === sWait1) {
        finalCycle := true.B
        state := sExec
      }
    }
    is(sExec) {
      //Execute instructions, increment IP as necessary
      //When we load the eend instruction, move to that state once execute pipeline is empty
      //Only the ordinary destination queue has to be empty. If mac dest queue is non-empty and fin is not asserted, that's also OK
      when(Oinst.pe === OtypePE.EXEC && Oinst.se === OtypeSE.END && Oinst.fmt === InstructionFMT.OTYPE
        && io.ctrl.empty && (io.ctrl.macEmpty || !io.fin)) {
        finalCycle := false.B //Deassert to avoid incrementing IP
        state := sEend
      }
    }
    is(sEend) {
      when(io.thread.stateIn === sEstart || io.thread.stateIn === sWait2) {
        finalCycle := true.B
        state := sStore
      }
    }
    is(sStore) {
      //Store data, increment IP as necessary
      when(Oinst.pe === OtypePE.PACKET && Oinst.se === OtypeSE.END) {
        state := sEnd
      }
    }
    is(sEnd) {
      when(!io.fin) {
        state := sLoad
        IP := 1.U
      } .otherwise {
        when(io.thread.stateIn === sExec || io.thread.stateIn === sEend) {
          //Other thread is still executing, go to wait until it's finished
          state := sWait2
        } .otherwise {
          //This is the final thread, go directly to idle
          state := sIdle
        }
      }
    }
    is(sWait1) {
      when(io.thread.stateIn === sEstart) {
        when(io.fin) {
          state := sWait2
        } .otherwise {
          state := sLoad
        }
      }
    }
    is(sWait2) {
      when(io.thread.stateIn === sEend) {
        state := sIdle
      }
    }
  }

  def assignRsRfValues(Rs1: UInt, Rs2: UInt, sv: UInt, rf1: RegisterFileType.Type, rf2: RegisterFileType.Type): Unit = {
    rs1.reg := Rs1
    rs1.subvec := sv
    rs1.rf := rf1
    rs2.reg := Rs2
    rs2.subvec := sv
    rs2.rf := rf2
  }
  //Output selection logic
  when(state === sExec && Rinst.fmt === InstructionFMT.RTYPE) {
  //It might be worthwhile factoring the output selection logic and the X,Y,Col update logic into separate blocks
  //Output logic can be run all the time without being gated behind the when-statement
    switch(Rinst.mod) {
      is(RtypeMod.VV) { //This won't work when processing SUM instructions. They are special cases
        //Output connections

        a := a_subvec(X)
        b := b_subvec(X)
//        for(i <- 0 until NUM_PROCELEM) {
//          a(i) := vRegFile.io.rdData1(i.U+X*NUM_PROCELEM.U)
//          b(i) := vRegFile.io.rdData2(i.U+X*NUM_PROCELEM.U)
//        }
        op := Rinst.op

        dest.reg := Mux(Rinst.op === MAC, Rinst.rd, v_rd) //MAC.VV instructions always end in s-registers
        dest.subvec := Mux(Rinst.op === MAC, 0.U, X)
        dest.rf := Mux(Rinst.op === MAC, SREG, VREG)
        macLimit := instrLen
        valid := true.B
        //Updates
        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick

        assignRsRfValues(v_rs1, v_rs2, X, VREG, VREG)
      }

      is(RtypeMod.XV) {
        //XV instructions take the first element in 'x' and operate with all elements in the first vector of 'v'
        //SlotSelect is used to index into xReg, X is used to index into vReg
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := xRegFile.io.rdData1(slotSelect)
//          b(i) := vRegFile.io.rdData2(i.U+X*NUM_PROCELEM.U)
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.reg := v_rd
        dest.subvec := X
        dest.rf := VREG
        valid := true.B

        //Updates
        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick

        assignRsRfValues(Rinst.rs1, v_rs2, X, XREG, VREG)
      }

      is(RtypeMod.XX) {
        //Operates in the same way as VV decode, except it only takes one clock cycle
        a := xRegFile.io.rdData1
        b := xRegFile.io.rdData2
        op := Rinst.op
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := XREG
        valid := true.B

        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, XREG, XREG)
      }

      is(RtypeMod.SV) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
//          b(i) := vRegFile.io.rdData2(i.U + X*NUM_PROCELEM.U)
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.reg := Mux(Rinst.op === MAC, Rinst.rd, v_rd)
        dest.subvec := Mux(Rinst.op === MAC, 0.U, X)
        dest.rf := Mux(Rinst.op === MAC, SREG, VREG)
        macLimit := instrLen
        valid := true.B
        //Updates
        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick

        assignRsRfValues(Rinst.rs1, v_rs2, X, SREG, VREG)
      }

      is(RtypeMod.SX) {
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
        }
        b := xRegFile.io.rdData2
        op := Rinst.op
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := XREG
        valid := true.B
        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, XREG)
      }

      is(RtypeMod.SS) {
        //Operates just like XX decode, except the same value is used on all ports
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
          b(i) := io.sRegFile.rdData2
        }
        op := Rinst.op
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := SREG
        valid := true.B
        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, SREG)
      }

      is(RtypeMod.KV) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := a_subvec(X)(col)
//          a(i) := vRegFile.io.rdData1(col + X*NUM_PROCELEM.U)
        }
        b := KE.io.keVals

        op := Rinst.op
        macLimit := KE_SIZE.U
        dest.reg := v_rd
        dest.subvec := Y
        dest.rf := VREG
        valid := true.B

        val colTick = col === (NUM_PROCELEM - 1).U
        val Xtick = X === (KE_SIZE / NUM_PROCELEM - 1).U
        val Ytick = Y === (KE_SIZE / NUM_PROCELEM - 1).U
        val SStick = slotSelect === (VREG_SLOT_WIDTH - 1).U
        //Updates
        col := Mux(colTick, 0.U, col + 1.U)
        X := Mux(colTick, Mux(Xtick, 0.U, X + 1.U), X)
        Y := Mux(Xtick && colTick, Mux(Ytick, 0.U, Y + 1.U), Y)
        slotSelect := Mux(Xtick && Ytick && colTick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)

        finalCycle := Xtick && Ytick && colTick && SStick
        //Rs/rf-values have no relevance here. Setting both source to XREG removes any chance of
        assignRsRfValues(v_rs1, X, Y, VREG, KREG)
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
    valid := false.B
    state := state
  }
}

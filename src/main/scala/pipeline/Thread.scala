package pipeline

import chisel3._
import ThreadState._
import chisel3.util._
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import vector.KEWrapper
import vector.Opcode._
import chisel3.experimental.BundleLiterals._
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
  /** State that this thread is currently in */
  val stateOut = Output(ThreadState())
  /** The literal value of the state. Only used for debugging purposes since we cannot peek enums yet */
  val stateOutUint = Output(UInt(8.W))
  /** State that the other thread is currently in */
  val stateIn = Input(ThreadState())
  /** Connections to the execute stage */
  val ex = new IdExIO
  /** Connections to memory stage */
  val mem = new IdMemIO
  /** Connections to the control module */
  val ctrl = new ThreadControlIO
}

/**
 * A module representing a thread in the decode stage. Each decode stage contains two threads
 * @param id The ID of the thread. If id=0, this thread will be the first to fetch data from memory.
 *           If ID=1, this thread will wait until thread 0 has started executing, at which point this one will start loading
 */
class Thread(id: Int) extends Module {
  require(id == 0 || id == 1, "Thread ID must be 0 or 1")
  val io = IO(new ThreadIO)

  // --- CONSTANTS ---
  private val numSubVectors: Int = SUBVECTORS_PER_VREG

  // --- MODULES ---
  /** Vector register file. Has [[NUM_VECTOR_REGISTERS]] entries, each of which holds [[VECTOR_REGISTER_DEPTH]] elements */
  val vRegFile = Module(new VectorRegisterFile(NUM_VECTOR_REGISTERS, VECTOR_REGISTER_DEPTH, VECTOR_REGISTER_DEPTH))
  /** X-value vector register file. Has [[NUM_VECTOR_REGISTERS]]/[[NUM_VREG_SLOTS]] entries, each of which holds [[VREG_SLOT_WIDTH]] values */
  val xRegFile = Module(new VectorRegisterFile(NUM_X_REG, VREG_SLOT_WIDTH, VREG_SLOT_WIDTH))
  /** Scalar register file. Has [[NUM_SCALAR_REGISTERS]] entries */
  val sRegFile = Module(new ScalarRegisterFile)
  /** Wrapper for KE-matrix, holding all KE-values */
  val KE = Module(new KEWrapper(NUM_PROCELEM, sync=false, SIMULATION))

  // --- REGISTERS ---
  /** Instruction buffer */
//  val iBuffer = RegInit(VecInit(Seq.fill(16)(0.U(32.W))))
  /** Instruction pointer into the instruction buffer */
  val IP = RegInit(0.U(4.W))
  /** Number of instruction in the current instruction buffer */
//  val iCount = RegInit(0.U(4.W))
  /** Current state for the instruction load/execution FSM */
//  val iloadStateReg = RegInit(sIdle)
  /** Current index into subvectors. Also gives the x-coordinate of the submatrix in the KE matrix */
  val X = RegInit(0.U(log2Ceil(VECTOR_REGISTER_DEPTH).W))
  /** Current y-coordinate used to index into KE matrix */
  val Y = RegInit(0.U(log2Ceil(KE_SIZE/NUM_PROCELEM).W))
  /** Current column of submatrix (x,y) in the KE matrix */
  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM).W))
  /** Used to select which vector from a vector slot is output */
  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH).W))
  /** State register */
  val state = RegInit(sIdle)

  // --- WIRES AND SIGNALS ---
  val currentInstr: UInt = io.instr

  val Rinst = currentInstr.asTypeOf(new RtypeInstruction)
  val Oinst = currentInstr.asTypeOf(new OtypeInstruction)
  val Sinst = currentInstr.asTypeOf(new StypeInstruction)
  val slot1 = Rinst.rs1 //Vector slot accessed by rs1
  val slot2 = Rinst.rs2 //Vector slot accessed by rs2

  /** Asserted whenever the first cycle of an instruction is being processed. Used by the control unit */
  val firstCycle = (X === 0.U) && (Y === 0.U) && (col === 0.U) && (slotSelect === 0.U)

  /** Used to indicate whenever an instruction is finished and the IP should update */
  val finalCycle = WireInit(false.B)


  //Generate subvectors for selection
  //This makes it easier to address subvectors of the output vectors a, b from register file
  val a_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(numSubVectors, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until numSubVectors) {
    a_subvec(i) := vRegFile.io.rdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := vRegFile.io.rdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }

  /** 'a' data subvector going into execute stage */
  val a = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W)))) //TODO Change default assignment to a_subvec(0)
  /** 'b' data subvector going to execute stage */
  val b = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
  /** Opcode going into execution stage */
  val op = WireDefault(NOP)
  /** Destination register for current operation */
  val dest = WireDefault((new Destination).Lit(_.rd -> 0.U, _.subvec -> 0.U, _.rf -> VREG))
  /** Limit for MAC operations, if such a one is being processed */
  val macLimit = WireDefault(0.U(32.W))
  /** Signals that the outgoing operation should be added to the destination queue */
  val newDest = WireDefault(false.B)

  //Generate vector register selects based on slot defined in instruction + slot select
  val v_rs1 = (slot1 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  val v_rs2 = (slot2 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  val v_rd = (Rinst.rd << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect


  //This should be done my moving the output logic in here, and updating the FSM in the decode stage
  //The decoder FSM still loads data from IM into instructionBuffer, and then sends instructions to threads
  //Has i,j,k registers keeping track of the base i,j,k-pair (and offsets?), which are passed into threads
  //Should forward writeback-results to the register banks which are currently active

  // --- OUTPUT & MODULE CONNECTIONS ---
  //Register file and KE connections
  vRegFile.io.rs1 := v_rs1
  vRegFile.io.rs2 := v_rs2
  xRegFile.io.rs1 := slot1
  xRegFile.io.rs2 := slot2
  sRegFile.io.rs1 := slot1
  sRegFile.io.rs2 := slot2
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

  //Outputs
  io.ex.a := a
  io.ex.b := b
  io.ex.dest:= dest
  io.ex.op := op
  io.ex.macLimit := macLimit
  io.ex.newDest := newDest
  io.ctrl.rtypemod := Rinst.mod
  io.ctrl.state := state
  io.ctrl.finalCycle := finalCycle
  io.ctrl.firstCycle := firstCycle
  io.ctrl.op := op
  io.stateOut := state

  io.ip := IP

  /** Debug values */
  //TODO Remove these
  io.stateOutUint := state.asUInt()
  io.ctrl.stateUint := state.asUInt()
  io.ex.dest.rfUint := dest.rf.asUInt()


  // --- LOGIC ---
  //IP update logic, default. Overridden by assignments in next state logic
  IP := Mux(finalCycle, IP + 1.U, IP)

  //Next state logic
  switch(state) {
    is(sIdle) {
      IP := 0.U
      when(Oinst.iev === OtypeIEV.INSTR && Oinst.se === OtypeSE.START && Oinst.fmt === InstructionFMT.OTYPE && io.start) {
        IP := 1.U
        if(id == 0) {
          state := sLoad
        } else {
          state := sWait1
        }
      }
    }
    is(sLoad) {
      //Load data for this instruction, increment pointer as necessary
      when(Oinst.iev === OtypeIEV.ELEM && Oinst.se === OtypeSE.START) {
        finalCycle := false.B //This is funky, but required for correct functionality
        state := sEstart
      }
    }
    is(sEstart) {
      when(io.stateIn === sEend || io.stateIn === sWait1) {
        finalCycle := true.B
        state := sExec
      }
    }
    is(sExec) {
      finalCycle := true.B
      //TODO We want to take a sneak peek at the next instruction. Perhaps another input field nextInstr?
      //Execute instructions, increment IP as necessary
      when(Oinst.iev === OtypeIEV.ELEM && Oinst.se === OtypeSE.END && Oinst.fmt === InstructionFMT.OTYPE ) {
        finalCycle := false.B
//        newDest := false.B
        state := sEend
      }
    }
    is(sEend) {
      //TODO Should not leave eend when instructions are still on their way back from execute stage
      when(io.stateIn === sEstart || io.stateIn === sWait2) {
        finalCycle := true.B
        state := sStore
      }
    }
    is(sStore) {
      //Store data, increment IP as necessary
      when(Oinst.iev === OtypeIEV.INSTR && Oinst.se === OtypeSE.END) {
        state := sIend
      }
    }
    is(sIend) {
      when(!io.fin) {
        state := sLoad
        IP := 1.U
      } .otherwise {
        when(io.stateIn === sExec || io.stateIn === sEend) {
          //Other thread is still executing, go to wait until it's finished
          state := sWait2
        } .otherwise {
          //This is the final thread, go directly to idle
          state := sIdle
        }
      }
    }
    is(sWait1) {
      when(io.stateIn === sEstart) {
        when(io.fin) {
          state := sWait2
        } .otherwise {
          state := sLoad
        }
      }
    }
    is(sWait2) {
      when(io.stateIn === sEend) {
        state := sIdle
      }
    }
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
        op := Rinst.op
        dest.rd := v_rd
        dest.subvec := X
        dest.rf := VREG
        //TODO This is not the correct way of setting these values. MacLImit should be much higher (nelem or ndof)
        macLimit := Mux(Rinst.op === MAC, (NUM_VREG_SLOTS * VECTOR_REGISTER_DEPTH / NUM_PROCELEM).U, 0.U)

        newDest := Mux(Rinst.op === MAC, slotSelect === 0.U && X === 0.U, Mux(Rinst.op === MAC, false.B, true.B))

        //Updates
        val Xtick = X === (numSubVectors - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      is(RtypeMod.XV) {
        //XV instructions take the first element in 'x' and operate with all elements in the first vector of 'v'
        //SlotSelect is used to index into xReg, X is used to index into vReg
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := xRegFile.io.rdData1(slotSelect)
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.rd := v_rd
        dest.subvec := X
        dest.rf := VREG
        newDest := true.B

        //Updates
        val Xtick = X === (numSubVectors - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      is(RtypeMod.XX) {
        //Operates in the same way as VV decode, except it only takes one clock cycle
        a := xRegFile.io.rdData1
        b := xRegFile.io.rdData2
        op := Rinst.op
        dest.rd := Rinst.rd
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
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := sRegFile.io.rdData1
        }
        b := b_subvec(X)
        op := Rinst.op
        dest.rd := v_rd
        dest.subvec := X
        dest.rf := VREG
        newDest := true.B
        //Updates
        val Xtick = X === (numSubVectors - 1).U
        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U

        X := Mux(Xtick, 0.U, X + 1.U)
        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
        finalCycle := Xtick && SStick
      }

      //TODO do we also want SX instructions?
      //If we reduce the register size, we can increase the MOD field to 4 bits
      //Might also make for easier decode logic
      //Right operand determines how x,y,col values are updated, left operand simply follows and outputs
      is(RtypeMod.SX) {
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := sRegFile.io.rdData1
        }
        b := xRegFile.io.rdData2
        op := Rinst.op
        dest.rd := Rinst.rd
        dest.subvec := 0.U
        dest.rf := XREG
        newDest := true.B
        finalCycle := true.B
      }

      is(RtypeMod.SS) {
        //Operates just like XX decode, except the same value is used on all ports
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := sRegFile.io.rdData1
          b(i) := sRegFile.io.rdData2
        }
        op := Rinst.op
        dest.rd := Rinst.rd
        dest.subvec := 0.U
        dest.rf := SREG
        newDest := true.B
        finalCycle := true.B
      }

      is(RtypeMod.MVP) {
        a := KE.io.keVals
        for (i <- 0 until NUM_PROCELEM) {
          b(i) := a_subvec(X)(col) //Notice: We're using a_subvec since we're fetching via rs1 and not rs2
        }
        op := MAC
        macLimit := KE_SIZE.U
        dest.rd := v_rd
        dest.subvec := Y
        newDest := (X === 0.U) && (col === 0.U)

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
    state := state
  }

}

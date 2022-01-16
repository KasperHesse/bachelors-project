package execution

import chisel3._
import ThreadState._
import chisel3.util._
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import Opcode._
import chisel3.experimental.BundleLiterals._
import memory.{IJKBundle, IJKgeneratorBundle}
import execution.RegisterFileType._

/**
 * I/O Ports for "Thread" modules inside of the decode stage
 */
class ThreadIO extends Bundle {
  /** The current instruction, fetched from instruction buffer in decode stage */
  val instr = Input(UInt(INSTRUCTION_WIDTH.W))
  /** Finished flag. Asserted when processing is finished and the thread should move to idle state after writing back */
  val fin = Input(Bool())
  /** Start flag. Asserted when a packet has been transferred from IM to instructionBuffer, and processing may begin */
  val start = Input(Bool())
  /** Instruction pointer of the current packet. Sent to IM in Decode stage */
  val ip = Output(UInt(log2Ceil(INSTRUCTION_BUFFER_SIZE+1).W))
  /** Connections to other thread in Decode stage */
  val threadOut = Output(new ThreadThreadIO)
  /** Input connections from other thread in Decode stage */
  val threadIn = Flipped(new ThreadThreadIO)
  /** The literal value of the state. Only used for debugging purposes since we cannot peek enums yet */
  val stateOutUint = Output(UInt(8.W))
  /** Connections to the execute stage */
  val ex = new IdExIO
  /** Connections to memory stage */
  val mem = new IdMemIO
  /** Connections to the control module */
  val ctrl = new ThreadControlIO
  /** Connections to writeback stage, both memory writeback and execute writeback */
  val wb = Flipped(new WbIdIO)
  /** Connections to shared register file in Decode stage */
  val sRegFile = Flipped(new ScalarRegFileIO)
}

/**
 * I/O ports connecting two threads. Use as-is on the output port of a thread, Use Flipped() on the input port
 */
class ThreadThreadIO extends Bundle {
  /** Current state of this thread */
  val state = Output(ThreadState())
  /** Current i,j,k values in the thread */
  val ijk = Output(new IJKgeneratorBundle)
}

/**
 * A module representing a thread in the decode stage. Each decode stage contains two threads. Implements [[ThreadIO]]
 * @param id The ID of the thread. If id=0, this thread will be the first to fetch data from memory.
 *           If ID=1, this thread will wait until thread 0 has started executing, at which point this one will start loading from memory
 */
class Thread(id: Int) extends Module {
  require(id == 0 || id == 1, "Thread ID must be 0 or 1")
  val io = IO(new ThreadIO)

  // --- MODULES ---
  val vRegFile = new InlineVectorRegisterFile(width=NUM_VREG, depth=VREG_DEPTH, id)
  val xRegFile = new InlineVectorRegisterFile(width=NUM_XREG, depth=XREG_DEPTH, id)
  if(SIMULATION) {
    vRegFile.initMemory()
    xRegFile.initMemory()
  }
  /** Module containing all transformed versions of the KE matrix */
  val KE = Module(new KEMatrix(sync = true))
  /** Immediate generator */
  val immGen = Module(new ImmediateGenerator)
  /** Module handling all memory access related stuff */
  val memAccess = Module(new ThreadMemoryAccess())

  // --- REGISTERS ---
  /** Instruction pointer into the instruction buffer */
  val IP = RegInit(0.U(log2Ceil(INSTRUCTION_BUFFER_SIZE+1).W))
  /** Current index into subvectors. Also gives the x-coordinate of the submatrix in the KE matrix */
  val X = RegInit(0.U(log2Ceil(VREG_DEPTH+1).W))
  /** Current y-coordinate used to index into KE matrix */
  val Y = RegInit(0.U(log2Ceil(KE_SIZE/NUM_PROCELEM+1).W))
  /** Current column of submatrix (x,y) in the KE matrix */
  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM+1).W))
  /** Used to select which vector from a vector slot is output */
  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH+1).W))
  /** State register */
  val state = RegInit(sIdle)
  /** Number of multiply-accumulate cycles to perform before a result is generated in MAC and RED instructions */
  val macLimit = WireDefault(0.U(log2Ceil(NDOFLENGTH/NUM_PROCELEM+1).W))
  /** Maximum memory index that should be accessed when performing ld.vec and st.vec */
  val maxIndex = RegInit(0.U(log2Ceil(NDOFSIZE+1).W))
  /** O-type length of currently executing instruction packet */
  val instrLen = RegInit(OtypeLen.SINGLE)
  /** Iteration value associated with all V-register files. Used when processing mac.kv instructions */
  val vRegIter = RegInit(VecInit(Seq.fill(VREG_SLOT_WIDTH)(0.U(2.W))))

  // --- WIRES AND SIGNALS ---
  /** Handle to the current instruction */
  val currentInstr: UInt = io.instr

  /** LUT to determine maximum index to access for ld.vec and st.vec operations */
   //If eg. NDOFSIZE != NDOFLENGTH, the system will start loading/storing values from the next vector in memory.
   //To avoid this, when index >= maxIndex, the vector index generator will make the indices invalid, causing it to load 0's and not perform stores from/to memory
  val maxIndexDecode = VecInit(Array(
    NDOFSIZE.U, //len == NDOF.
    1.U, //len is invalid
    ELEMS_PER_VSLOT.U, //len == SINGLE
    (2*ELEMS_PER_VSLOT).U, //len == DOUBLE
    NELEMSIZE.U, //len == NELEMVEC
    0.U,
    0.U,
    0.U
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
  /** 'a' data subvector going into execute stage */
  val a = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W)))) //TODO Change default assignment to a_subvec(0)
  /** 'b' data subvector going to execute stage */
  val b: Vec[SInt] = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
  /** Write data going into write queue when performing store operations */
  val wrData = WireDefault(VecInit(Seq.fill(NUM_MEMORY_BANKS)(0.S(FIXED_WIDTH.W))))
  /** Bundle holding the values necessary to identify register source 1 */
  val rs1 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
  /** Bundle holding the values necessary to identify register source 2 */
  val rs2 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
  /** Opcode going into execution stage */
  val op = Mux(Rinst.fmt === InstructionFMT.RTYPE, Rinst.op, NOP)
  /** Destination register for current operation */
  val dest = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.subvec -> 0.U, _.rf -> VREG))
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
  val vrs1 = Mux(state === sStore, memAccess.io.rsrd, v_rs1) //Only in sStore should we use memAccess to select from this one
  val vRegRdData1 = vRegFile.setReadPort(vrs1)
  val vRegRdData2 = vRegFile.setReadPort(v_rs2)
  vRegFile.setWritePort(io.wb.rd.reg, io.wb.wrData, io.wb.we && io.wb.rd.rf === RegisterFileType.VREG)

  //This makes it easier to address subvectors of the output vectors from register file
  val a_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val wrData_subvec = Wire(Vec(VREG_DEPTH/NUM_MEMORY_BANKS, Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until SUBVECTORS_PER_VREG) {
    a_subvec(i) := vRegRdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := vRegRdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }
  for(i <- 0 until VREG_DEPTH/NUM_MEMORY_BANKS) {
    wrData_subvec(i) := vRegRdData1.slice(i*NUM_MEMORY_BANKS, (i+1)*NUM_MEMORY_BANKS)
  }

  val xrs1 = Mux(state === sStore, Sinst.rsrd, Rinst.rs1) //Only in sStore should we use the register in S-instruction
  val xRegRdData1 = xRegFile.setReadPort(xrs1)
  val xRegRdData2 = xRegFile.setReadPort(Rinst.rs2)
  xRegFile.setWritePort(io.wb.rd.reg, VecInit(io.wb.wrData.slice(0, XREG_DEPTH)), io.wb.we && io.wb.rd.rf === RegisterFileType.XREG)

  io.sRegFile.rs1 := Rinst.rs1
  io.sRegFile.rs2 := Rinst.rs2

  //Connect X, Y, col, iter inputs to KE matrix
  KE.io.keX := X
  KE.io.keY := Y
  KE.io.keCol := col
  KE.io.keIter := vRegIter(slotSelect)

  //Immediate generator
  immGen.io.instr := Rinst

  //Memory access module
  memAccess.io.instr := Sinst
  memAccess.io.threadState := state
  memAccess.io.maxIndex := maxIndex
  memAccess.io.ijkIn := io.threadIn.ijk
  memAccess.io.otherThreadState := io.threadIn.state

  //These inputs to scalar reg file are supplied via writeback stage, hence these are dontcares
  io.sRegFile.we := DontCare
  io.sRegFile.rd := DontCare
  io.sRegFile.wrData := DontCare

  //Outputs
  //Execute
  io.ex.a := a
  io.ex.b := b
  io.ex.dest := dest
  io.ex.op := op
  io.ex.macLimit := macLimit
  io.ex.valid := valid
  io.ex.rs1 := rs1
  io.ex.rs2 := rs2
  io.ex.imm := immGen.io.imm
  io.ex.useImm := Rinst.immflag

  //Control
  io.ctrl.rtypemod := Rinst.mod
  io.ctrl.state := state
  io.ctrl.op := op
  io.ctrl.rs1 := rs1
  io.ctrl.rs2 := rs2
  io.ctrl.fmt := Rinst.fmt
  io.ctrl.finalCycle := finalCycle
  io.ctrl.firstCycle := firstCycle

  //Memory
  io.mem.vec <> memAccess.io.vec
  io.mem.edof <> memAccess.io.edof
  io.mem.readQueue <> memAccess.io.readQueue
  io.mem.neighbour <> memAccess.io.neighbour
  io.mem.ls := Mux(state === sStore, StypeLoadStore.STORE, StypeLoadStore.LOAD)
  io.mem.writeQueue.bits.wrData := wrData
  io.mem.writeQueue.bits.mod := RegNext(Sinst.mod) //Must delay by one clock cycle since register file reads are delayed
  io.mem.writeQueue.bits.iter := RegNext(memAccess.io.wqIter)
  io.mem.writeQueue.valid := RegNext(memAccess.io.wqValid)

  //Other thread
  io.threadOut.state := state
  io.threadOut.ijk := memAccess.io.ijkOut

  //Decode
  io.ip := IP

  /** Debug values */
  //TODO Remove these
  io.stateOutUint := state.asUInt()
  io.ctrl.stateUint := state.asUInt()
  io.ex.dest.rfUint := dest.rf.asUInt()
  io.ex.opUInt := op.asUInt()


  // --- LOGIC ---
  //IP update logic, default. Overridden by assignments in next state logic
  IP := Mux(finalCycle, IP + 1.U, IP)

  //Next state logic
  switch(state) {
    is(sIdle) {
      IP := 0.U
      when(Oinst.mod === OtypeMod.PACKET && Oinst.se === OtypeSE.START && Oinst.fmt === InstructionFMT.OTYPE && io.start) {
        IP := 1.U
        maxIndex := maxIndexDecode(Oinst.len.asUInt())
        instrLen := Oinst.len
        if(id == 0) {
          state := sLoad
        } else {
          state := sWait1
        }
      }
    }
    is(sLoad) {
      finalCycle := memAccess.io.finalCycle //Memory access module handles the rest
      //Load data for this instruction, increment pointer as necessary
      when(Oinst.mod === OtypeMod.EXEC && Oinst.se === OtypeSE.START) {
        finalCycle := false.B //This is funky, but required for correct functionality
        //Above is necessary since we should keep the IP when waiting in sEstart
        state := sEstart
      }
    }
    is(sEstart) {
      when(io.threadIn.state === sEend || io.threadIn.state === sWait1) {
        finalCycle := true.B
        state := sExec
      }
    }
    is(sExec) {
      //Execute instructions, increment IP as necessary
      //When we load the eend instruction, move to that state once execute pipeline is empty
      //Only the ordinary destination queue has to be empty. If mac dest queue is non-empty and fin is not asserted, that's also OK
      when(Oinst.mod === OtypeMod.EXEC && Oinst.se === OtypeSE.END && Oinst.fmt === InstructionFMT.OTYPE
        && io.ctrl.empty && (io.ctrl.macEmpty || !io.fin)) {
        finalCycle := false.B //Deassert to avoid incrementing IP
        state := sEend
      }
    }
    is(sEend) {
      when((io.threadIn.state === sEstart || io.threadIn.state === sWait2) && memAccess.io.ijkOut.valid) {
        finalCycle := true.B
        state := sStore
      }
    }
    is(sStore) {
      finalCycle := memAccess.io.finalCycle //Memory access module handles the remaining logic
      when(Oinst.mod === OtypeMod.PACKET && Oinst.se === OtypeSE.END && Oinst.fmt === InstructionFMT.OTYPE) {
        state := sPend
      }
    }
    is(sPend) {
      when(!io.fin) {
        state := sLoad
        IP := 1.U
      } .otherwise {
        when(io.threadIn.state === sExec || io.threadIn.state === sEend) {
          //Other thread is still executing, go to wait until it's finished
          state := sWait2
        } .otherwise {
          //This is the final thread, go directly to idle
          state := sIdle
        }
      }
    }
    is(sWait1) {
      when(io.threadIn.state =/= sLoad) {
        when(io.fin) {
          state := sWait2
        } .elsewhen(io.threadIn.ijk.valid) {
          state := sLoad //Must not move into this state before other thread has ijk values ready
        }
      }
    }
    is(sWait2) {
      when(io.threadIn.state === sStore) {
        state := sIdle
      }
    }
  }

  /**
   * Assigns register values going to execute module for forwarding, and control module for data hazard avoidance
   * @param Rs1 The register file index of the first operand
   * @param Rs2 The register file index of the second operand
   * @param sv The subvector value of both operands
   * @param rf1 The register file type of the first operand
   * @param rf2 The register file type of the second operand
   */
  def assignRsRfValues(Rs1: UInt, Rs2: UInt, sv: UInt, rf1: RegisterFileType.Type, rf2: RegisterFileType.Type): Unit = {
    rs1.reg := Rs1
    rs1.subvec := sv
    rs1.rf := rf1
    rs2.reg := Rs2
    rs2.subvec := sv
    rs2.rf := rf2
  }

  //Output update logic in exec state
  when(state === sExec && Rinst.fmt === InstructionFMT.RTYPE) {
    switch(Rinst.mod) {
      is(RtypeMod.VV) {
        dest.reg := Mux(Rinst.op === MAC || Rinst.op === RED, Rinst.rd, v_rd) //MAC.VV and RED.VV instructions end in s / x-registers respectively.
        dest.subvec := Mux(Rinst.op === MAC || Rinst.op === RED, 0.U, X)
        dest.rf := Mux(Rinst.op === MAC, SREG, Mux(Rinst.op === RED, XREG, VREG))
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
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := Mux(Rinst.op === RED, SREG, XREG)
        valid := true.B

        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, XREG, XREG)
      }
      is(RtypeMod.SV) {
        dest.reg := Mux(Rinst.op === MAC, Rinst.rd, v_rd)
        dest.subvec := Mux(Rinst.op === MAC, 0.U, X)
        dest.rf := Mux(Rinst.op === MAC, SREG, VREG)
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
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := XREG
        valid := true.B
        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, XREG)
      }

      is(RtypeMod.SS) {
        dest.reg := Rinst.rd
        dest.subvec := 0.U
        dest.rf := SREG
        valid := true.B
        finalCycle := true.B
        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, SREG)
      }

      is(RtypeMod.KV) {
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
        //Rs/rf-values have no relevance here, but must be assigned to avoid latches
        assignRsRfValues(v_rs1, X, Y, VREG, KREG)
      }
    }
  }

  //Output selection logic in exec state
  when(state === sExec && RegNext(Rinst.fmt) === InstructionFMT.RTYPE) {
    switch(RegNext(Rinst.mod)) {
      is(RtypeMod.VV) {
        a := a_subvec(RegNext(X))
        b := b_subvec(RegNext(X))
      }
      is(RtypeMod.XV) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := xRegRdData1(RegNext(slotSelect))
        }
        b := b_subvec(RegNext(X))
      }
      is(RtypeMod.SV) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
        }
        b := b_subvec(RegNext(X))
      }
      is(RtypeMod.XX) {
        a := xRegRdData1
        b := xRegRdData2
      }
      is(RtypeMod.SX) {
        for(i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
        }
        b := xRegRdData2
      }
      is(RtypeMod.SS) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := io.sRegFile.rdData1
          b(i) := io.sRegFile.rdData2
        }
      }
      is(RtypeMod.KV) {
        for (i <- 0 until NUM_PROCELEM) {
          a(i) := a_subvec(RegNext(X))(RegNext(col))
        }
        b := KE.io.keVals
      }
    }
  }

  //MAC limit selection
  when(Rinst.op === RED && Rinst.mod === RtypeMod.VV) {
    macLimit := (VREG_DEPTH/NUM_PROCELEM).U
  } .elsewhen(Rinst.op === RED && Rinst.mod === RtypeMod.XX) {
    macLimit := 1.U
  } .elsewhen(Rinst.op === MAC && Rinst.mod === RtypeMod.KV) {
    macLimit := KE_SIZE.U
  } .elsewhen(Rinst.op === MAC && (Rinst.mod === RtypeMod.VV || Rinst.mod === RtypeMod.SV)) {
    //Performing MAC.VV/MAC.SV/MAC.IV instructions should only take place in lengths NDOF and NELEMVEC
    when(instrLen === OtypeLen.NDOF) {
      macLimit := (NDOFLENGTH/NUM_PROCELEM).U
    } .otherwise {
      macLimit := (NELEMLENGTH/NUM_PROCELEM).U
    }
  }


  //Write data selection when storing
  when(state === sStore && RegNext(Rinst.fmt) === InstructionFMT.STYPE) {
    switch(RegNext(Sinst.mod)) {
      is(StypeMod.VEC) {
        wrData := wrData_subvec(RegNext(memAccess.io.subvec))
      }
      is(StypeMod.SEL) {
        wrData := VecInit(Seq.fill(NUM_MEMORY_BANKS)(xRegRdData1(0)))
      }
      is(StypeMod.ELEM) {
        wrData(0) := xRegRdData1(RegNext(memAccess.io.subvec))
      }
      is(StypeMod.DOF) {
        wrData := wrData_subvec(RegNext(memAccess.io.subvec))
      }
      is(StypeMod.FDOF) {
        wrData := wrData_subvec(RegNext(memAccess.io.subvec))
      }
    }
  }

  //Vreg iteration value update when loading DOFs
  when(state === sLoad && memAccess.io.edof.valid && io.mem.edof.ready && memAccess.io.readQueue.bits.mod === StypeMod.DOF) {
    //KE0,KE7 / KE1,KE6 / KE2,KE5 and KE3,KE4 have the contents, so we only need 4 KE matrices for proper functionality.
    //Using a simple LUT to map all iteration values to values in range [0:3]
    val itermap = Wire(Vec(8, UInt(2.W)))
    for(i <- 0 until 8) {
      itermap(i) := {if (i < 4) i.U(2.W) else (7-i).U(2.W)}
    }
    vRegIter(memAccess.io.slotSelect) := itermap(memAccess.io.readQueue.bits.iter)
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
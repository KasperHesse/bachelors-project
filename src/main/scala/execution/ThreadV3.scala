//package pipeline
//
//import chisel3._
//import ThreadState._
//import chisel3.util._
//import utils.Config._
//import utils.Fixed.FIXED_WIDTH
//import vector.KEWrapper
//import vector.Opcode._
//import chisel3.experimental.BundleLiterals._
//import memory.{IJKBundle, IJKgeneratorBundle}
//import pipeline.RegisterFileType._
//
//
///**
// * A module representing a thread in the decode stage. Each decode stage contains two threads. Implements [[ThreadIO]]
// * @param id The ID of the thread. If id=0, this thread will be the first to fetch data from memory.
// *           If ID=1, this thread will wait until thread 0 has started executing, at which point this one will start loading
// */
//class ThreadV3(id: Int) extends Module {
//  require(id == 0 || id == 1, "Thread ID must be 0 or 1")
//  val io = IO(new ThreadIO)
//
//  // --- MODULES ---
////  /** Vector register file. Has [[utils.Config.NUM_VREG]] entries, each of which holds [[utils.Config.VREG_DEPTH]] elements */
////  val vRegFile = Module(new VectorRegisterFile(NUM_VREG, VREG_DEPTH, VREG_DEPTH))
////  /** X-value vector register file. Has [[utils.Config.NUM_XREG]] entries, each of which holds [[utils.Config.VREG_SLOT_WIDTH]] values */
////  val xRegFile = Module(new VectorRegisterFile(NUM_XREG, VREG_SLOT_WIDTH, VREG_SLOT_WIDTH))
//  val vRegFile = new InlineVectorRegisterFile(width=NUM_VREG, depth=VREG_DEPTH, memInitFileLocation = s"src/resources/meminit/vreg$id.txt")
//  val xRegFile = new InlineVectorRegisterFile(width=NUM_XREG, depth=XREG_DEPTH, memInitFileLocation = s"src/resources/meminit/xreg$id.txt")
//  if(SIMULATION) {
//    vRegFile.initMemory()
//    xRegFile.initMemory()
//  }
//  /** Wrapper for KE-matrix, holding all KE-values */
//  val KE = Module(new KEWrapper(NUM_PROCELEM, sync=true, SIMULATION))
//  /** Immediate generator */
//  val immGen = Module(new ImmediateGenerator)
//  /** Module handling all memory access related stuff */
//  val memAccess = Module(new ThreadMemoryAccess())
//
//  // --- REGISTERS ---
//  /** Instruction pointer into the instruction buffer */
//  val IP = RegInit(0.U(4.W))
//  /** Current index into subvectors. Also gives the x-coordinate of the submatrix in the KE matrix */
//  val X = RegInit(0.U(log2Ceil(VREG_DEPTH).W))
//  //  val X = RegInit(0.U)
//  /** Current y-coordinate used to index into KE matrix */
//  val Y = RegInit(0.U(log2Ceil(KE_SIZE/NUM_PROCELEM).W))
//  //  val Y = RegInit(0.U)
//  /** Current column of submatrix (x,y) in the KE matrix */
//  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM).W))
//  //  val col = RegInit(0.U)
//  /** Used to select which vector from a vector slot is output */
//  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH).W))
//  //  val slotSelect = RegInit(0.U)
//  /** State register */
//  val state = RegInit(sIdle)
//
//  // --- WIRES AND SIGNALS ---
//  /** Handle to the current instruction */
//  val currentInstr: UInt = io.instr
//  /** Execution length of the instruction being decoded */
//  val instrLen = RegInit(1.U(log2Ceil(NDOFLENGTH+1).W))
//  /** LUT to decode vector lengths (for setting macLimit) */
//  val lenDecode = VecInit(Array(
//    (NDOFLENGTH/NUM_PROCELEM).U,
//    (NELEMLENGTH/NUM_PROCELEM).U,
//    (ELEMS_PER_VSLOT/NUM_PROCELEM).U //allows us to perform a single mac instruction on stored values
//  ))
//
//  val Rinst = currentInstr.asTypeOf(new RtypeInstruction)
//  val Oinst = currentInstr.asTypeOf(new OtypeInstruction)
//  val Sinst = currentInstr.asTypeOf(new StypeInstruction)
//
//  /** VREG slot as specified by rs1 */
//  val slot1 = Rinst.rs1
//  /** VREG slot as specified by rs2 */
//  val slot2 = Rinst.rs2
//  /** Asserted whenever the first cycle of an instruction is being processed. Used by the control unit */
//  val firstCycle = IP =/= RegNext(IP)
//  /** Used to indicate whenever an instruction is finished and the IP should update */
//  val finalCycle = WireDefault(false.B)
//  /** 'a' data subvector going into execute stage */
//  val a = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W)))) //TODO Change default assignment to a_subvec(0)
//  /** 'b' data subvector going to execute stage */
//  val b = WireDefault(VecInit(Seq.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))))
//  /** Bundle holding the values necessary to identify register source 1 */
//  val rs1 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
//  /** Bundle holding the values necessary to identify register source 2 */
//  val rs2 = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> VREG, _.subvec -> 0.U, _.rfUint -> 0.U))
//  /** Opcode going into execution stage */
////  val op = WireDefault(NOP)
//  val op = Mux(Rinst.fmt === InstructionFMT.RTYPE, Rinst.op, NOP)
//  /** Destination register for current operation */
//  val dest = WireDefault((new RegisterBundle).Lit(_.reg -> 0.U, _.subvec -> 0.U, _.rf -> VREG))
//  /** Limit for MAC operations, if such a one is being processed */
//  val macLimit = WireDefault(0.U(32.W))
//  /** Signals that the outgoing operation should be added to the destination queue */
//  val valid = WireDefault(false.B)
//
//  //Declare variables here to allow assignment in if-statement
//  /** Index into vector register file */
//  val v_rs1 = Wire(UInt(log2Ceil(NUM_VREG+1).W))
//  /** Index into vector register file */
//  val v_rs2 = Wire(UInt(log2Ceil(NUM_VREG+1).W))
//  /** Index into vector register file */
//  val v_rd = Wire(UInt(log2Ceil(NUM_VREG+1).W))
//  //Generate vector register selects based on slot defined in instruction + slot select
//  //Use lookup tables to avoid multiplications in case we don't have a power of two
//  if(!isPow2(VREG_SLOT_WIDTH)) {
//    val vRegLookup = Wire(Vec(3, Vec(NUM_VREG_SLOTS, UInt(log2Ceil(NUM_VREG+1).W))))
//    for(i <- 0 until NUM_VREG_SLOTS) {
//      vRegLookup(0)(i) := (i*VREG_SLOT_WIDTH).U
//      vRegLookup(1)(i) := (i*VREG_SLOT_WIDTH).U
//      vRegLookup(2)(i) := (i*VREG_SLOT_WIDTH).U
//    }
//    v_rs1 := vRegLookup(0)(slot1) + slotSelect
//    v_rs2 := vRegLookup(1)(slot2) + slotSelect
//    v_rd := vRegLookup(2)(Rinst.rd) + slotSelect
//  } else {
//    v_rs1 := (slot1 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
//    v_rs2 := (slot2 << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
//    v_rd := (Rinst.rd << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
//  }
//
//
//
//
//  // --- OUTPUT & MODULE CONNECTIONS ---
//  val vRegRdData1 = vRegFile.setReadPort(v_rs1)
//  val vRegRdData2 = vRegFile.setReadPort(v_rs2)
//  vRegFile.setWritePort(io.wb.rd.reg, io.wb.wrData, io.wb.we && io.wb.rd.rf === RegisterFileType.VREG)
//  //This makes it easier to address subvectors of the output vectors a, b from register file
//  val a_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
//  val b_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
//  for(i <- 0 until SUBVECTORS_PER_VREG) {
//    a_subvec(i) := vRegRdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
//    b_subvec(i) := vRegRdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
//  }
//
//  val xRegRdData1 = xRegFile.setReadPort(Rinst.rs1)
//  val xRegRdData2 = xRegFile.setReadPort(Rinst.rs2)
//  xRegFile.setWritePort(io.wb.rd.reg, VecInit(io.wb.wrData.slice(0, XREG_DEPTH)), io.wb.we && io.wb.rd.rf === RegisterFileType.XREG)
//
//  io.sRegFile.rs1 := Rinst.rs1
//  io.sRegFile.rs2 := Rinst.rs2
//
//  KE.io.keX := X
//  KE.io.keY := Y
//  KE.io.keCol := col
//
//  //Immediate generator
//  immGen.io.instr := Rinst
//
//
//  //Memory access module
//  memAccess.io.instr := Sinst
//  memAccess.io.threadState := state
//  memAccess.io.maxIndex := 512.U //TODO bad value, how to set this?
//
//  //All inputs to scalar reg file are supplied via writeback stage, hence these are dontcares
//  io.sRegFile.we := DontCare
//  io.sRegFile.rd := DontCare
//  io.sRegFile.wrData := DontCare
//
//  //Outputs
//  //Execute
//  io.ex.a := a
//  io.ex.b := b
//  io.ex.dest := dest
//  io.ex.op := op
//  io.ex.macLimit := macLimit
//  io.ex.valid := valid
//  io.ex.rs1 := rs1
//  io.ex.rs2 := rs2
//  io.ex.imm := immGen.io.imm
//  io.ex.useImm := Rinst.immflag
//
//  //Control
//  io.ctrl.rtypemod := Rinst.mod
//  io.ctrl.state := state
//  io.ctrl.op := op
//  io.ctrl.rs1 := rs1
//  io.ctrl.rs2 := rs2
//  io.ctrl.fmt := Rinst.fmt
//  io.ctrl.finalCycle := finalCycle
//  io.ctrl.firstCycle := firstCycle
//
//  //Memory
//  io.mem.vec <> memAccess.io.vec
//  io.mem.edof <> memAccess.io.edof
//  io.mem.readQueue <> memAccess.io.readQueue
//  io.mem.neighbour <> memAccess.io.neighbour
//  io.mem.ls := Sinst.ls
//  io.threadOut.state := state
//  io.threadOut.ijk := memAccess.io.ijkOut
//  memAccess.io.ijkIn := io.threadIn.ijk
//
//  //Others
//  io.threadOut.state := state
//  io.ip := IP
//
//  /** Debug values */
//  //TODO Remove these
//  io.stateOutUint := state.asUInt()
//  io.ctrl.stateUint := state.asUInt()
//  io.ex.dest.rfUint := dest.rf.asUInt()
//  io.ex.opUInt := op.asUInt()
//  io.mem.wrData.bits := DontCare
//  io.mem.wrData.valid := false.B
//
//
//  // --- LOGIC ---
//  //IP update logic, default. Overridden by assignments in next state logic
//  IP := Mux(finalCycle, IP + 1.U, IP)
//
//  //Next state logic
//  switch(state) {
//    is(sIdle) {
//      IP := 0.U
//      when(Oinst.pe === OtypePE.PACKET && Oinst.se === OtypeSE.START && Oinst.fmt === InstructionFMT.OTYPE && io.start) {
//        IP := 1.U
//        instrLen := lenDecode(Oinst.len.asUInt())
//        if(id == 0) {
//          state := sLoad
//        } else {
//          state := sWait1
//        }
//      }
//    }
//    is(sLoad) {
//      finalCycle := memAccess.io.finalCycle //Memory access module handles the rest
//      //Load data for this instruction, increment pointer as necessary
//      when(Oinst.pe === OtypePE.EXEC && Oinst.se === OtypeSE.START) {
//        finalCycle := false.B //This is funky, but required for correct functionality
//        //Above is necessary since we should keep the IP when waiting in sEstart
//        state := sEstart
//      }
//    }
//    is(sEstart) {
//      when(io.threadIn.state === sEend || io.threadIn.state === sWait1) {
//        finalCycle := true.B
//        state := sExec
//      }
//    }
//    is(sExec) {
//      //Execute instructions, increment IP as necessary
//      //When we load the eend instruction, move to that state once execute pipeline is empty
//      //Only the ordinary destination queue has to be empty. If mac dest queue is non-empty and fin is not asserted, that's also OK
//      when(Oinst.pe === OtypePE.EXEC && Oinst.se === OtypeSE.END && Oinst.fmt === InstructionFMT.OTYPE
//        && io.ctrl.empty && (io.ctrl.macEmpty || !io.fin)) {
//        finalCycle := false.B //Deassert to avoid incrementing IP
//        state := sEend
//      }
//    }
//    is(sEend) {
//      when(io.threadIn.state === sEstart || io.threadIn.state === sWait2) {
//        finalCycle := true.B
//        state := sStore
//      }
//    }
//    is(sStore) {
//      //Store data, increment IP as necessary
//      when(Oinst.pe === OtypePE.PACKET && Oinst.se === OtypeSE.END) {
//        state := sPend
//      }
//    }
//    is(sPend) {
//      when(!io.fin) {
//        state := sLoad
//        IP := 1.U
//      } .otherwise {
//        when(io.threadIn.state === sExec || io.threadIn.state === sEend) {
//          //Other thread is still executing, go to wait until it's finished
//          state := sWait2
//        } .otherwise {
//          //This is the final thread, go directly to idle
//          state := sIdle
//        }
//      }
//    }
//    is(sWait1) {
//      when(io.threadIn.state === sEstart) {
//        when(io.fin) {
//          state := sWait2
//        } .otherwise {
//          state := sLoad
//        }
//      }
//    }
//    is(sWait2) {
//      when(io.threadIn.state === sEend) {
//        state := sIdle
//      }
//    }
//  }
//
//  def assignRsRfValues(Rs1: UInt, Rs2: UInt, sv: UInt, rf1: RegisterFileType.Type, rf2: RegisterFileType.Type): Unit = {
//    rs1.reg := Rs1
//    rs1.subvec := sv
//    rs1.rf := rf1
//    rs2.reg := Rs2
//    rs2.subvec := sv
//    rs2.rf := rf2
//  }
//
//  //Must check both the current and previous value of Rinst, such that the final output from register file can leave
//  //thread once the final instruction has been processed.
//  when(state === sExec && Rinst.fmt === InstructionFMT.RTYPE) {
//    //It might be worthwhile factoring the output selection logic and the X,Y,Col update logic into separate blocks
//    //Output logic can be run all the time without being gated behind the when-statement
//    switch(Rinst.mod) {
//      is(RtypeMod.VV) { //This won't work when processing SUM instructions. They are special cases
//        //Output connections
//
////        a := a_subvec(RegNext(X))
////        b := b_subvec(RegNext(X))
////        op := Rinst.op
//
//        dest.reg := Mux(Rinst.op === MAC, Rinst.rd, v_rd) //MAC.VV instructions always end in s-registers
//        dest.subvec := Mux(Rinst.op === MAC, 0.U, X)
//        dest.rf := Mux(Rinst.op === MAC, SREG, VREG)
//        macLimit := instrLen
//        valid := true.B
//        //Updates
//        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
//        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U
//
//        X := Mux(Xtick, 0.U, X + 1.U)
//        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
//        finalCycle := Xtick && SStick
//
//        assignRsRfValues(v_rs1, v_rs2, X, VREG, VREG)
//      }
//
//      is(RtypeMod.XV) {
//        //XV instructions take the first element in 'x' and operate with all elements in the first vector of 'v'
//        //SlotSelect is used to index into xReg, X is used to index into vReg
////        for (i <- 0 until NUM_PROCELEM) {
////          a(i) := xRegRdData1(RegNext(slotSelect))
////        }
////        b := b_subvec(RegNext(X))
////        op := Rinst.op
//        dest.reg := v_rd
//        dest.subvec := X
//        dest.rf := VREG
//        valid := true.B
//
//        //Updates
//        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
//        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U
//
//        X := Mux(Xtick, 0.U, X + 1.U)
//        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
//        finalCycle := Xtick && SStick
//
//        assignRsRfValues(Rinst.rs1, v_rs2, X, XREG, VREG)
//      }
//
//      is(RtypeMod.XX) {
//        //Operates in the same way as VV decode, except it only takes one clock cycle
////        a := xRegRdData1
////        b := xRegRdData2
////        op := Rinst.op
//        dest.reg := Rinst.rd
//        dest.subvec := 0.U
//        dest.rf := XREG
//        valid := true.B
//
//        finalCycle := true.B
//        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, XREG, XREG)
//      }
//
//      is(RtypeMod.SV) {
////        for (i <- 0 until NUM_PROCELEM) {
////          a(i) := RegNext(io.sRegFile.rdData1) //TODO should probably just use memory for the sreg file as well. Right now, it is delayed by one cycle to match vector reg file
////        }
////        b := b_subvec(RegNext(X))
////        op := Rinst.op
//        dest.reg := Mux(Rinst.op === MAC, Rinst.rd, v_rd)
//        dest.subvec := Mux(Rinst.op === MAC, 0.U, X)
//        dest.rf := Mux(Rinst.op === MAC, SREG, VREG)
//        macLimit := instrLen
//        valid := true.B
//        //Updates
//        val Xtick = X === (SUBVECTORS_PER_VREG - 1).U
//        val SStick: Bool = slotSelect === (VREG_SLOT_WIDTH - 1).U
//
//        X := Mux(Xtick, 0.U, X + 1.U)
//        slotSelect := Mux(Xtick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
//        finalCycle := Xtick && SStick
//
//        assignRsRfValues(Rinst.rs1, v_rs2, X, SREG, VREG)
//      }
//      //Consider factoring the update logic into a separate switch statement from the output logic.
//      //Update logic takes current values of X,Y into accoutn
//      //Output logic takes previous values + previous value of Rinst into account
//
//      is(RtypeMod.SX) {
////        for(i <- 0 until NUM_PROCELEM) {
////          a(i) := RegNext(io.sRegFile.rdData1) //TODO use memory for sreg and remove this register
////        }
////        b := xRegRdData2
////        op := Rinst.op
//        dest.reg := Rinst.rd
//        dest.subvec := 0.U
//        dest.rf := XREG
//        valid := true.B
//        finalCycle := true.B
//        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, XREG)
//      }
//
//      is(RtypeMod.SS) {
//        //Operates just like XX decode, except the same value is used on all ports
////        for (i <- 0 until NUM_PROCELEM) {
////          a(i) := RegNext(io.sRegFile.rdData1) //TODO remove registers
////          b(i) := RegNext(io.sRegFile.rdData2)
////        }
////        op := Rinst.op
//        dest.reg := Rinst.rd
//        dest.subvec := 0.U
//        dest.rf := SREG
//        valid := true.B
//        finalCycle := true.B
//        assignRsRfValues(Rinst.rs1, Rinst.rs2, 0.U, SREG, SREG)
//      }
//
//      is(RtypeMod.KV) {
////        for (i <- 0 until NUM_PROCELEM) {
////          a(i) := a_subvec(RegNext(X))(RegNext(col))
////        }
////        b := KE.io.keVals
//
////        op := Rinst.op
//        macLimit := KE_SIZE.U
//        dest.reg := v_rd
//        dest.subvec := Y
//        dest.rf := VREG
//        valid := true.B
//
//        val colTick = col === (NUM_PROCELEM - 1).U
//        val Xtick = X === (KE_SIZE / NUM_PROCELEM - 1).U
//        val Ytick = Y === (KE_SIZE / NUM_PROCELEM - 1).U
//        val SStick = slotSelect === (VREG_SLOT_WIDTH - 1).U
//        //Updates
//        col := Mux(colTick, 0.U, col + 1.U)
//        X := Mux(colTick, Mux(Xtick, 0.U, X + 1.U), X)
//        Y := Mux(Xtick && colTick, Mux(Ytick, 0.U, Y + 1.U), Y)
//        slotSelect := Mux(Xtick && Ytick && colTick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
//
//        finalCycle := Xtick && Ytick && colTick && SStick
//        //Rs/rf-values have no relevance here. Setting both source to XREG removes any chance of
//        assignRsRfValues(v_rs1, X, Y, VREG, KREG)
//      }
//    }
//  }
//
//  when(state === sExec && RegNext(Rinst.fmt) === InstructionFMT.RTYPE) {
//    switch(RegNext(Rinst.mod)) {
//      is(RtypeMod.VV) {
//        a := a_subvec(RegNext(X))
//        b := b_subvec(RegNext(X))
//      }
//      is(RtypeMod.XV) {
//        for (i <- 0 until NUM_PROCELEM) {
//          a(i) := xRegRdData1(RegNext(slotSelect))
//        }
//        b := b_subvec(RegNext(X))
//      }
//      is(RtypeMod.SV) {
//        for (i <- 0 until NUM_PROCELEM) {
//          a(i) := RegNext(io.sRegFile.rdData1) //TODO should probably just use memory for the sreg file as well. Right now, it is delayed by one cycle to match vector reg file
//        }
//        b := b_subvec(RegNext(X))
//      }
//      is(RtypeMod.XX) {
//        a := xRegRdData1
//        b := xRegRdData2
//      }
//      is(RtypeMod.SX) {
//        for(i <- 0 until NUM_PROCELEM) {
//          a(i) := RegNext(io.sRegFile.rdData1) //TODO use memory for sreg and remove this register
//        }
//        b := xRegRdData2
//      }
//      is(RtypeMod.SS) {
//        for (i <- 0 until NUM_PROCELEM) {
//          a(i) := RegNext(io.sRegFile.rdData1) //TODO remove registers
//          b(i) := RegNext(io.sRegFile.rdData2)
//        }
//      }
//      is(RtypeMod.KV) {
//        for (i <- 0 until NUM_PROCELEM) {
//          a(i) := a_subvec(RegNext(X))(RegNext(col))
//        }
//        b := KE.io.keVals
//      }
//    }
//  }
//
//  //Stall management
//  when(io.ctrl.stall) {
//    X := X
//    slotSelect := slotSelect
//    Y := Y
//    col := col
//    IP := IP
//    valid := false.B
//    state := state
//  }
//}

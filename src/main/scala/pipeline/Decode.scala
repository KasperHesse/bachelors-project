package pipeline

import chisel3._
import chisel3.util._
import DecodeState._
import utils.Config._
import utils.Fixed._
import chisel3.experimental.BundleLiterals._

/**
 * I/O ports for the decode stage
 */
class DecodeIO extends Bundle {
  /** Connections to fetch stage */
  val fe = Flipped(new IfIdIO)
  /** Connections to execute stage */
  val ex = new IdExIO
  /** Output connections to memory stage */
  val mem = new IdMemIO
  /** Input connections from execute writeback stage */
  val wb = Flipped(new WbIdIO)
  /** Input connections from memory writeback stage */
  val memWb = Flipped(new WbIdIO)
  /** Connections to control module */
  val ctrl = new IdControlIO
}

/**
 * Main class for the decode stage. Contains two threads which take turns accessing memory and the execution stage.
 * Implements [[DecodeIO]]
 */
class Decode extends Module {
  val io = IO(new DecodeIO())

  // --- MODULES ---
  val threads = for(i <- 0 until 2) yield {
    Module(new Thread(i))
  }
  val sRegFile = Module(new ScalarRegisterFile("src/resources/meminit/sreg.hex.txt"))
  val branchTargetGen = Module(new BranchTargetGenerator)

  // --- REGISTERS ---
  /** Pipeline stage register */
  val fe_instr = RegNext(io.fe.instr)
  val fe_pc = RegNext(io.fe.pc)
  /** State register */
  val state = RegInit(DecodeState.sIdle)
  /** Instruction buffer */
  val iBuffer = for(i <- 0 until 2) yield {RegInit(VecInit(Seq.fill(INSTRUCTION_BUFFER_SIZE)(0.U(INSTRUCTION_WIDTH.W))))}
  /** Instruction pointer, used when filling iBuffer */
  val IP = RegInit(0.U(log2Ceil(INSTRUCTION_BUFFER_SIZE+1).W))
  /** Number of instructions in iBuffer */
  val iCount = RegInit(0.U(log2Ceil(INSTRUCTION_BUFFER_SIZE+1).W))
  /** Progress when accessing vectors / the number of elements that have been loaded so far. */
  val progress = RegInit(0.U(log2Ceil(NDOF+ELEMS_PER_VSLOT+1).W))
  /** Total number of operations to issue before instructions are finished / total number of elements to load */
  val maxProgress = RegInit(0.U(log2Ceil(NDOF+ELEMS_PER_VSLOT+1).W))
  /** The value by which progress counter should increment on every thread swap */
  val progressIncr = RegInit(0.U(log2Ceil(ELEMS_PER_VSLOT+1).W))
  /** ID of thread which is currently accessing the execute stage */
  val execThread = RegInit(1.U(1.W))
  /** ID of thread which is currently accessing memory */
  val memThread = RegInit(0.U(1.W))

  // --- WIRES AND SIGNALS ---
  /** Length of the current instruction */ //iBuffer(0) always holds an O-type instruction indicating packet length
  val instrLen = iBuffer(0).asTypeOf(new OtypeInstruction).len

  /** LUT to decode instruction lengths given in pstart */
  val lenDecode = VecInit(Array(
    NDOFLENGTH.U, //len == NDOF
    1.U, //Len is invalid
    1.U, //len == SINGLE
    1.U, //len is invalid
    leastMultiple(ELEMS_PER_VSLOT, NELEMSIZE).U, //len == NELEMVEC
    leastMultiple(XREG_DEPTH, NELEM).U, //len == NELEMDOF
    NELEM.U, //len == NELEMSTEP
    1.U //len is invalid
  ))
  /** LUT mapping instruction length to progress increment */
  val incrDecode = VecInit(Array(
    ELEMS_PER_VSLOT.U, //len == NDOF
    1.U, //len is invalid
    1.U, //len == SINGLE
    1.U, //Len is invalid
    ELEMS_PER_VSLOT.U, //len == NELEMVEC
    XREG_DEPTH.U, //len == NELEMDOF
    1.U, //len == NELEMSTEP
    1.U //len is invalid
  ))
  //When performing NDOF loads, we access VREG_DEPTH*VREG_SLOT_WIDTH elements per thread.
  //When performing i,j,k-based loads, we only load VREG_SLOT_WIDTH elements per thread.
  /** Current states of our threads */
  val threadStates = Wire(Vec(2, ThreadState()))
  /** Asserted to threads when the current instruction packet is finished */
  val fin = WireDefault(false.B)
  /** Asserted when threads should start executing */
  val start = WireDefault(false.B)
  /** Instruction at pipeline register, interpreted as B-type instruction */
  val Binst = fe_instr.asTypeOf(new BtypeInstruction)
  /** Branch signal going into fetch stage and control module */
  val branch = WireDefault(false.B)

  for(i <- 0 until 2) {
    threadStates(i) := threads(i).io.threadOut.state
  }

  // --- OUTPUTS AND CONNECTIONS --- //
  //Common thread connections
  for(i <- threads.indices) {
    val thread = threads(i)
    thread.io.fin := fin
    thread.io.start := start
    thread.io.sRegFile.rdData1 := sRegFile.io.rdData1
    thread.io.sRegFile.rdData2 := sRegFile.io.rdData2
    thread.io.instr := iBuffer(i)(thread.io.ip)
  }
  //Specific thread connections
  threads(0).io.threadIn := threads(1).io.threadOut
  threads(1).io.threadIn := threads(0).io.threadOut
  io.ctrl.threadCtrl(0) <> threads(0).io.ctrl
  io.ctrl.threadCtrl(1) <> threads(1).io.ctrl

  //Control module connections
  io.ctrl.state := state
  io.ctrl.execThread := execThread
  io.ctrl.branch := branch

  //Default connections to shared resources
  sRegFile.io.rd := io.wb.rd.reg
  sRegFile.io.wrData := io.wb.wrData(0)
  sRegFile.io.we := io.wb.we && io.wb.rd.rf === RegisterFileType.SREG

  branchTargetGen.io.instr := Binst
  branchTargetGen.io.pc := fe_pc
  io.fe.branchTarget := branchTargetGen.io.target
  io.fe.branch := branch

  //Debug connections. TODO: Remove these
  io.ctrl.stateUint := state.asUInt()

  //Assign shared resources to threads
  when(execThread === 0.U) {
    //Thread 0 accessed execute and wb stage
    threads(0).io.ex <> io.ex
    threads(0).io.wb <> io.wb
    sRegFile.io.rs1 := threads(0).io.sRegFile.rs1
    sRegFile.io.rs2 := threads(0).io.sRegFile.rs2
    threads(0).io.mem.writeQueue.ready := false.B
    threads(0).io.mem.edof.ready := false.B
    threads(0).io.mem.neighbour.ready := false.B
    threads(0).io.mem.vec.ready := false.B
    threads(0).io.mem.readQueue.ready := false.B

    //Thread 1 accesses mem stage
    threads(1).io.mem <> io.mem
    threads(1).io.wb <> io.memWb
  } .otherwise {
    //Thread 0 accesses mem stage
    threads(0).io.mem <> io.mem
    threads(0).io.wb <> io.memWb

    //Thread 1 accesses execute and wb stage
    threads(1).io.ex <> io.ex
    threads(1).io.wb <> io.wb
    sRegFile.io.rs1 := threads(1).io.sRegFile.rs1
    sRegFile.io.rs2 := threads(1).io.sRegFile.rs2
    threads(1).io.mem.writeQueue.ready := false.B
    threads(1).io.mem.edof.ready := false.B
    threads(1).io.mem.neighbour.ready := false.B
    threads(1).io.mem.vec.ready := false.B
    threads(1).io.mem.readQueue.ready := false.B
  }

  // --- LOGIC ---
  //Next state logic
  switch(state) {
    is(sIdle) {
      when(io.ctrl.iload && !branch) {
        state := sLoad
      }
    }
    is(sLoad) {
      iBuffer(0)(IP) := fe_instr
      iBuffer(1)(IP) := fe_instr
      IP := IP + 1.U
      when(!io.ctrl.iload) {
        state := sExec
        IP := 0.U
        iCount := IP
        start := true.B

        maxProgress := lenDecode(instrLen.asUInt()) //TODO ugly code, introduce some kind of error checking here?
        progressIncr := incrDecode(instrLen.asUInt())
        progress := 0.U
      }
    }
    is(sExec) {
      //Asserted when memthread and execthread swap roles
      val swapThreads = (threadStates(memThread) === ThreadState.sEstart || threadStates(memThread) === ThreadState.sWait2)
          .&& (threadStates(execThread) === ThreadState.sEend || threadStates(execThread) === ThreadState.sWait1)

      when(swapThreads) { //Progress is incremented whenever memThread moves from estart to exec
        progress := Mux(progress >= maxProgress, progress, progress + progressIncr)
        execThread := memThread
        memThread := execThread
      }

      //Single-length instructions are always about to be finished, and the secondary thread shouldn't do anything
      when(progress >= maxProgress || instrLen === OtypeLen.SINGLE) {
        fin := true.B
      }
      when(threadStates(0) === ThreadState.sIdle && threadStates(1) === ThreadState.sIdle) {
        state := sIdle
        execThread := 1.U
        memThread := 0.U
      }
    }
  }

  //Branch instruction logic
  when(Binst.fmt === InstructionFMT.BTYPE && state === sIdle) {
    sRegFile.io.rs1 := Binst.rs1
    sRegFile.io.rs2 := Binst.rs2

    when(Binst.comp === BranchComp.EQUAL) {
      branch := sRegFile.io.rdData1 === sRegFile.io.rdData2
    } .elsewhen(Binst.comp === BranchComp.NEQ) {
      branch := sRegFile.io.rdData1 =/= sRegFile.io.rdData2
    } .elsewhen(Binst.comp === BranchComp.GEQ) {
      branch := sRegFile.io.rdData1 >= sRegFile.io.rdData2
    } .elsewhen(Binst.comp === BranchComp.LT) {
      branch := sRegFile.io.rdData1 < sRegFile.io.rdData2
    }
  }

  //Stall logic
  when(io.ctrl.stall) {
    state := state
    IP := IP
    progress := progress
    maxProgress := maxProgress
  }
}

package pipeline

import chisel3._
import chisel3.util._
import DecodeState._
import utils.Config._

/**
 * Main class for the decode stage. Contains two threads which take turns accessing memory and the execution stage.
 * Implements [[DecodeOldIO]].
 */
class Decode extends Module {
  val io = IO(new DecodeIO())

  // --- MODULES ---
  val threads = for(i <- 0 until 2) yield {
    Module(new Thread(i))
  }

  // --- REGISTERS ---
  /** Pipeline stage register */
  val in = RegNext(io.in)
  /** State register */
  val state = RegInit(DecodeState.sIdle)
  /** Instruction buffer */
  val iBuffer = RegInit(VecInit(Seq.fill(16)(0.U(INSTRUCTION_WIDTH.W))))
  /** Instruction pointer, used when filling iBuffer */
  val IP = RegInit(0.U(4.W))
  /** Number of instructions in iBuffer */
  val iCount = RegInit(0.U(4.W))
  /** i element used for indexing */
  val i = RegInit(0.U(log2Ceil(NELX+1).W))
  /** j element used for indexing */
  val j = RegInit(0.U(log2Ceil(NELY+1).W))
  /** k element used for indexing */
  val k = RegInit(0.U(log2Ceil(NELZ+1).W))
  /** Progress when accessing vectors in linear fashion */
  val progress = RegInit(0.U(log2Ceil(NDOF+1).W))
  /** Total number of operations to issue before instructions are finished */
  val maxProgress = RegInit(0.U(log2Ceil(NDOF+1).W))
  /** Asserted when the current instruction packet is finished */
  val fin = WireDefault(false.B)
  /** Asserted when threads should start executing */
  val start = WireDefault(false.B)

  /** Which thread is currently accessing the execute stage */
  val execThread = RegInit(1.U(1.W))
  /** Which thread is currently accessing memory */
  val memThread = RegInit(0.U(1.W))

  // --- WIRES AND SIGNALS ---

  val instrLen = iBuffer(0).asTypeOf(new OtypeInstruction).len
  /** LUT to decode vector lengths given in istart */
  val lenDecode = VecInit(Array(NDOF.U, NELEM.U, 1.U))
  /** Vector for easier access to the current state of our executing threads */
  val threadStates = Wire(Vec(2, ThreadState()))
  /** Vector for easier access to execute outputs of each thread */
  val threadExes = Wire(Vec(2, new IdExIO))
  /** Vector for easier access to memory outputs of each thread */
  val threadMems = Wire(Vec(2, new IdMemIO))

  for(i <- 0 until 2) {
    threadStates(i) := threads(i).io.stateOut
    threadExes(i) := threads(i).io.ex
    threadMems(i) := threads(i).io.mem
  }

  // --- OUTPUTS AND CONNECTIONS --- //
  threads(0).io.instr := iBuffer(threads(0).io.ip)
  threads(1).io.instr := iBuffer(threads(1).io.ip)

  for(thread <- threads) {
    thread.io.i := i
    thread.io.j := j
    thread.io.k := k
    thread.io.progress := progress
    thread.io.fin := fin
    thread.io.start := start
    thread.io.mem.rdData := DontCare
  }
  threads(0).io.stateIn := threads(1).io.stateOut
  threads(1).io.stateIn := threads(0).io.stateOut
  io.threadCtrl(0) <> threads(0).io.ctrl
  io.threadCtrl(1) <> threads(1).io.ctrl

  io.ctrl.state := state
  io.ex <> threadExes(execThread)
  io.mem <> threadMems(memThread)
  // --- LOGIC ---

  //Next state logic
  switch(state) {
    is(sIdle) {
      when(io.ctrl.iload) {
        state := sLoad
      }
    }
    is(sLoad) {
      iBuffer(IP) := in.instr
      IP := IP + 1.U
      when(!io.ctrl.iload) {
        state := sExec
        IP := 0.U
        iCount := IP
        start := true.B
        //iBuffer(0) always holds an O-type instruction indicating packet length
        maxProgress := lenDecode(instrLen.asUInt())
      }
    }
    is(sExec) {
      val swapThreads = (threadStates(memThread) === ThreadState.sEstart || threadStates(memThread) === ThreadState.sWait2)
          .&& (threadStates(execThread) === ThreadState.sEend || threadStates(execThread) === ThreadState.sWait1)
     //Whenever mem/exec thread swap position, increment progress counter
      //We can increment progress counter as soon as memThread enters estart

      when(swapThreads) {
        progress := progress + NUM_PROCELEM.U
        execThread := memThread
        memThread := execThread
      }

      when(progress >= maxProgress || instrLen === OtypeLen.SINGLE) {
        fin := true.B
      }
      when(threadStates(0) === ThreadState.sIdle && threadStates(1) === ThreadState.sIdle) {
        state := sFinalize //Probably not a necessary state
      }
    }
    is(sFinalize) {
      fin := true.B
      start := false.B
    }
  }

  //Stall logic
  when(io.ctrl.stall) {
    state := state
    IP := IP
    i := i
    j := j
    k := k
    progress := progress
    maxProgress := maxProgress
  }

}

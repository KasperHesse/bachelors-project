package execution

import chisel3._
import execution.RegisterBundle

class ControlIO extends Bundle {
  val id = Flipped(new IdControlIO)
  val ex = Flipped(new ExControlIO)
  val fe = Flipped(new IfControlIO)
  val mem = Flipped(new MemControlIO)
}

class Control extends Module {
  val io = IO(new ControlIO)

  // --- WIRES AND SHORTHANDS ---
  /** Instruction load signal to decode stage. This register is used to keep the value high while loading */
  val iload = RegInit(false.B)
  /** Execution stall register. Used to hold the estall value for multiple cycles */
  val estall = RegInit(false.B)
  /** Memory stall register. Used to hold memory stall value for multiple clock cycles */
  val hasWaited = RegInit(false.B)
  /** Shortcut to executing thread's control signal */
  val execThread = io.id.threadCtrl(io.id.execThread)
  /** Shortcut to memory access thread's control signals */
  val memThread = io.id.threadCtrl(io.id.execThread + 1.U(1.W)) //Adding one should lap back to 0/inc to 1
  /** Stall signal going into execute stage and executing thread */
  val execStall = WireDefault(false.B)
  /** Stall signal going into memory stage and memory access thread */
  val memStall = WireDefault(false.B)


  // --- CONNECTIONS ---
  //We need these assignments or the firrtl checker will be angry
  io.id.threadCtrl(0).stall := false.B
  io.id.threadCtrl(1).stall := false.B
  io.id.threadCtrl(0).empty := io.ex.empty
  io.id.threadCtrl(1).empty := io.ex.empty
  io.id.threadCtrl(0).macEmpty := io.ex.macEmpty
  io.id.threadCtrl(1).macEmpty := io.ex.macEmpty
  io.id.stall := false.B

  io.fe.iload := false.B
  io.id.iload := false.B

  execThread.stall := execStall
  io.ex.stall := execStall //normal verison
  memThread.stall := memStall

  // --- LOGIC ---
  //Logic signals for easier decode of O-type instructions

  val Oinst = io.id.instr.asTypeOf(new OtypeInstruction)
  val isPacket: Bool = Oinst.mod === OtypeMod.PACKET
  val isTime: Bool = Oinst.mod === OtypeMod.TIME
  val isEnd = Oinst.se === OtypeSE.END
  val isOtype: Bool = Oinst.fmt === InstructionFMT.OTYPE
  val isBtype: Bool = Oinst.fmt === InstructionFMT.BTYPE
  val isStart: Bool = Oinst.se === OtypeSE.START

  // --- INSTRUCTION FETCH/DECODE CONTROL SIGNALS ---
  //When idling and instructions are available, load them in
  when((isStart && isPacket && isOtype && io.id.state === DecodeState.sIdle) || iload) {
    io.id.iload := true.B
    io.fe.iload := true.B
    iload := true.B
  }
  //When we read the final instruction, stop loading after this one
  when(isEnd && isPacket && isOtype && io.id.state === DecodeState.sLoad) {
//    io.id.iload := false.B
//    io.fe.iload := false.B
    iload := false.B
  }
  //When handling branch or tstart/tend instructions, step past that instruction once triggered
  when(io.id.state === DecodeState.sBranch || (isOtype && isTime && io.id.state === DecodeState.sIdle)) { //Must perform this check to avoid doubled instructions in sim
    io.fe.iload := true.B
  }

/*
  //When we read the final instruction, stop loading after this one
  when(isEnd && isPacket && isOtype && io.id.state === DecodeState.sLoad) {
    io.id.iload := false.B
    io.fe.iload := false.B
    iload := false.B
    //When we spot a pstart instruction, or we're currently loading a packet, keep processing
  } .elsewhen((isStart && isPacket && isOtype && io.id.state === DecodeState.sIdle) || iload) {
    io.id.iload := true.B
    io.fe.iload := true.B
    iload := true.B
    //When not executing or processing a branch, toggle fetch iload
  } .elsewhen(!(io.id.state === DecodeState.sExec || io.id.state === DecodeState.sBranch2)) {
    io.fe.iload := true.B
  }
  */





  // --- THREAD CONTROL SIGNALS ---
  //Stall until read or write queue is empty once the final read/write operation has been issued
  when(memThread.state === ThreadState.sLoad && io.mem.rqCount =/= 0.U && memThread.fmt =/= InstructionFMT.STYPE) {
    memThread.stall := true.B
  } .elsewhen(memThread.state === ThreadState.sStore && memThread.fmt =/= InstructionFMT.STYPE && (io.mem.wqCount =/= 0.U || !hasWaited) ) {
    memThread.stall := true.B
  }

  when(memThread.state === ThreadState.sStore && memThread.fmt =/= InstructionFMT.STYPE && !hasWaited) {
    hasWaited := true.B
  } .elsewhen(memThread.state =/= ThreadState.sStore) {
    hasWaited := false.B
  }
  //When memthread performs nothing but at st.sel, we don't have time to fill the write queue before moving on
  //To fix this: either introduce a delay in control module, or wait one clock cycle if fmt =/= stype


  // --- EXECUTE STAGE STALLS ---
  //These stall signals are active if the upcoming instruction relies on data that is currently being computed but not yet finished (data hazards)
  val dataHazardStall = WireDefault(false.B)
  val dataHazardVec =  for(eha <- io.ex.queueHead) yield {
    eha.valid && eha.dest.subvec === execThread.rs1.subvec && ((eha.dest.rf === execThread.rs1.rf && eha.dest.reg === execThread.rs1.reg) || (eha.dest.rf === execThread.rs2.rf && eha.dest.reg === execThread.rs2.reg))
  }
  when(dataHazardVec.reduce((a, b) => a|b) && execThread.state === ThreadState.sExec) {
    dataHazardStall := true.B
  }

  //When decoded instruction is not the same opcode as instruction in execute stage, stall execution decode until destination queue is empty
  val destQueueStall = WireDefault(false.B)
  val validHead = {for(head <- io.ex.queueHead) yield { //We don't need to stall if queue is non-empty but no entries in dQueue are invalid (this happens when the final value in dQueue is being output)
    head.valid
  }}.reduce((a,b) => a|b)
  when(execThread.state === ThreadState.sExec && execThread.op =/= io.ex.op && !io.ex.empty && validHead) {
    destQueueStall := true.B
  }

  //When decoding XX, SX and SS instructions, stall for one clock cycle if the executing instruction
  //does not exactly match the decoding instruction. This ensures proper arrival into destination queue
  //See DecExWbSpec random instruction mix with seed -1059893295096578903L for why this is necessary
  val isSingleCycleOp: Bool = (execThread.rtypemod === RtypeMod.XX) || (execThread.rtypemod === RtypeMod.SX) || (execThread.rtypemod === RtypeMod.SS)
  val newOp: Bool = execThread.op =/= RegNext(execThread.op) && execThread.rtypemod =/= RegNext(execThread.rtypemod)
  val singleCycleAwaitStall = WireDefault(false.B)
  when((isSingleCycleOp && newOp && execThread.state === ThreadState.sExec)) {
    singleCycleAwaitStall := true.B
//    estall := true.B
//  } .elsewhen (estall) {
//    estall := false.B
  }

  //When exec thread has entered Eend state but data is still in either destination queue, stall until empty
  execStall := dataHazardStall | destQueueStall | singleCycleAwaitStall | estall

  //TODO figure out why the f decode/execute isn't working.
  /*
  Observations: In the correct version, there is 1 clock cycle delay between singleCycleAwaitStall and destQueueStall going high
  since destQueueStall is delayed by one clock cycle, the DATA coming in on the next instruction is able to override the correct data
  Namely, the values (6,18) are held for 2 cycles in the correct version, and only for 1 cycle in the wrong version, before being replaced with (1,432).
  To solve the problem, we must hold the data for one addtional clock cycle.
  Solution: Change the input register from a RegNext to a RegEnable?
   */
}

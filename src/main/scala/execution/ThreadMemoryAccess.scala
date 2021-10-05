package execution

import chisel3._
import chisel3.util._
import memory.{AddressGenProducerIO, IJKgeneratorBundle, IJKgeneratorConsumerIO, ReadQueueBundle}
import utils.Config._
import ThreadState._
import MemoryAccessFSMState._
import execution.RegisterFileType._
import execution.StypeMod._

class ThreadMemoryAccessIO extends Bundle {
  /** Instruction from Thread module */
  val instr = Input(new StypeInstruction)
  /** Current state of parent Thread module */
  val threadState = Input(ThreadState())
  /** Current state of the other Thread module */
  val otherThreadState = Input(ThreadState())
  /** Maximum index at which the VEC index generator should output validIndex=1. Depends on instruction length in thread */
  val maxIndex = Input(UInt(log2Ceil(NDOFLENGTH + 1).W))
  /** Indices used when performing .vec operations that go straight to address generator */
  val vec = Decoupled(new AddressGenProducerIO)
  /** Values used when performing .dof and .fdof operations that go to EDOF generator */
  val edof = Decoupled(new IJKgeneratorConsumerIO)
  /** Values used when performing .elem, .fcn, .edn1, .edn2 and .sel operations that go to neighbour generator */
  val neighbour = Decoupled(new IJKgeneratorConsumerIO)
  /** Output connections to read queue in memory stage */
  val readQueue = Decoupled(new ReadQueueBundle)
  /** I,J,K values passed to other thread */
  val ijkOut = Output(new IJKgeneratorBundle)
  /** I,J,K values input from other thread */
  val ijkIn = Flipped(new IJKgeneratorBundle)
  /** Signal output on the final cycle of a load instruction, indicating that the Thread's IP can update */
  val finalCycle = Output(Bool())
  /** Register select signal used when processing store instructions */
  val rsrd = Output(UInt(log2Ceil(NUM_VREG+1).W))
  /** Iteration value for write queue output (assembled in thread) */
  val wqIter = Output(UInt(3.W))
  /** Subvector value, used to select the correct subvector from a vector register when performing st.vec, st.dof and st.fdof instruction */
  val subvec = Output(UInt(log2Ceil(XREG_DEPTH+1).W))
  /** Valid signal for write queue */
  val wqValid = Output(Bool())
}

/**
 * This is the main module used for interfacing between a thread and the memory stage.
 * Contains the submodules for generating IJK values and indices for VEC lookups.
 * This module primarily controls ready/valid signalling with the memory module
 *
 * @param sim Flag that must be enabled when this module is tested by itself. When tested/instantiated inside of threads,
 *            this parameter must be set false
 */
class ThreadMemoryAccess(sim: Boolean = false) extends Module {
  val io = IO(new ThreadMemoryAccessIO)

  /** Signal used to restart the address generators when moving to thread idle state */
  val resetGenerators = io.threadState === ThreadState.sIdle

  // --- MODULES ---
  //Declaring both modules with additional reset signal to make our lives easier
  val ijk = withReset(resetGenerators | reset.asBool()) {Module(new IJKGeneratorFSM)}
  val vec = withReset(resetGenerators | reset.asBool()) {Module(new VecMemoryFSM)}

  // --- REGISTERS ---

  // --- SIGNALS ---
  val Sinstr = io.instr
  val readQueue = Wire(new ReadQueueBundle)
  val queueValidFlag = WireDefault(false.B)
  /** Destination register for VEC and DOF instructions */
  val vecRd = WireDefault(0.U(log2Ceil(NUM_VREG+1).W))
  /** Vector register slot when processing DOF or VEC instructions */
  val slotSelect = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH+1).W))
  /** The number of read queue bundles that have been transmitted for the current instruction */
  val outputCnt = RegInit(0.U(log2Ceil(NUM_XREG+1).W))
  /** Pulled high  */
  val finalCycle = WireDefault(false.B)

  val threadState = if(sim) RegNext(io.threadState) else io.threadState

  val outputState = (threadState === sLoad || threadState === sStore) && io.instr.fmt === InstructionFMT.STYPE

  // --- LOGIC ---
  //Generate logic to create correct read queue bundles
  readQueue.rd.rf := Mux(Sinstr.mod === StypeMod.VEC || Sinstr.mod === StypeMod.DOF, VREG, XREG)
  readQueue.rd.rfUint := readQueue.rd.rf.asUInt() //TODO debug signal, remove this
  readQueue.iter := ijk.io.ijkOut.iteration
  readQueue.mod := Sinstr.mod
  readQueue.rd.subvec := 0.U
  readQueue.rd.reg := Mux(Sinstr.mod === DOF || Sinstr.mod === FDOF || Sinstr.mod === VEC, vecRd, Sinstr.rsrd)

  //Generate destination register value for VEC and DOF instructions
  if(!isPow2(VREG_SLOT_WIDTH)) {
    val vRegLookup = Wire(Vec(NUM_VREG_SLOTS, UInt(log2Ceil(NUM_VREG+1).W)))
    for(i <- 0 until NUM_VREG_SLOTS) {
      vRegLookup(i) := (i*VREG_SLOT_WIDTH).U
    }
    vecRd := vRegLookup(Sinstr.rsrd) + slotSelect
  } else {
    vecRd := (Sinstr.rsrd << log2Ceil(VREG_SLOT_WIDTH)).asUInt + slotSelect
  }

  //Logic to drive read queue bundles and increment slotSelect on instructions that require multiple bundles
  when(Sinstr.mod === VEC && outputState) {
    //Increment outputcnt every time a vec output is made.
    val ocTick = outputCnt === ((VREG_DEPTH/NUM_MEMORY_BANKS)-1).U
    val SStick = slotSelect === (VREG_SLOT_WIDTH-1).U
    outputCnt := Mux(vec.io.vec.valid, Mux(ocTick, 0.U, outputCnt + 1.U), outputCnt)
    slotSelect := Mux(ocTick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
    queueValidFlag := vec.io.vec.valid && io.vec.ready
    finalCycle := ocTick && SStick & vec.io.vec.valid && io.vec.ready

  } .elsewhen((Sinstr.mod === DOF || Sinstr.mod === FDOF) && outputState) {
    //Increment outputCnt when first output is made + 2 times afterwards for 3 total bundles per. vreg
    val ocTick = outputCnt === ((VREG_DEPTH/NUM_MEMORY_BANKS)-1).U
    val SStick = slotSelect === (VREG_SLOT_WIDTH-1).U
    //Outputcnt should start incrementing when valid & ready
    outputCnt := Mux((ijk.io.edof.valid && io.edof.ready) || outputCnt > 0.U, Mux(ocTick, 0.U, outputCnt + 1.U), outputCnt)
    slotSelect := Mux(ocTick, Mux(SStick, 0.U, slotSelect + 1.U), slotSelect)
    queueValidFlag := (ijk.io.edof.valid && io.edof.ready) || outputCnt > 0.U
    finalCycle := ocTick && SStick

  } .elsewhen((Sinstr.mod === FCN || Sinstr.mod === EDN1 || Sinstr.mod === EDN2) && outputState) {
    //Increment outputCnt when first output is made + once afterwards for 2 total bundles per instruction
    val ocTick = outputCnt === 1.U
    outputCnt := Mux((ijk.io.neighbour.valid && io.neighbour.ready) || outputCnt > 0.U, Mux(ocTick, 0.U, outputCnt + 1.U), outputCnt)
    queueValidFlag := (ijk.io.neighbour.valid && io.neighbour.ready) | outputCnt > 0.U
    finalCycle := ocTick

  } .elsewhen(Sinstr.mod === SEL && outputState) {
    queueValidFlag := ijk.io.neighbour.valid && io.neighbour.ready
    finalCycle := ijk.io.neighbour.valid && io.neighbour.ready

  } .elsewhen(Sinstr.mod === ELEM && outputState) {
    val ocTick = outputCnt === (XREG_DEPTH-1).U
    outputCnt := Mux(ijk.io.neighbour.valid && io.neighbour.ready, Mux(ocTick, 0.U, outputCnt + 1.U), outputCnt)
    finalCycle := ocTick && ijk.io.neighbour.valid && io.neighbour.ready
    queueValidFlag := ijk.io.neighbour.valid && io.neighbour.ready
  }

  // --- CONNECTIONS ---
  io.vec <> vec.io.vec
  io.edof <> ijk.io.edof
  io.neighbour <> ijk.io.neighbour
  io.ijkOut := ijk.io.ijkOut

  ijk.io.ijkIn := io.ijkIn
  ijk.io.instr := Sinstr
  ijk.io.threadState := threadState
  ijk.io.otherThreadState := io.otherThreadState

  vec.io.threadState := threadState
  vec.io.instr := Sinstr
  vec.io.maxIndex := io.maxIndex
  vec.io.otherThreadState := io.otherThreadState

  io.finalCycle := finalCycle
  io.readQueue.bits := readQueue
  io.readQueue.valid := queueValidFlag && io.threadState === sLoad

  io.wqIter := ijk.io.ijkOut.iteration
  io.subvec := outputCnt
  io.rsrd := Mux(Sinstr.mod === DOF || Sinstr.mod === FDOF || Sinstr.mod === VEC, vecRd, Sinstr.rsrd)
  io.wqValid := queueValidFlag && io.threadState === sStore

  //Override ready/valid handshakes to only be valid when an S-type instruction is processing
  when(Sinstr.fmt =/= InstructionFMT.STYPE) {
    io.edof.valid := false.B
    io.neighbour.valid := false.B
    io.vec.valid := false.B
    io.readQueue.valid := false.B

    ijk.io.edof.ready := false.B
    ijk.io.neighbour.ready := false.B
    vec.io.vec.ready := false.B
  }
}



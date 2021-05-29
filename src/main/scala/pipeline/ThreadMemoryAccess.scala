package pipeline

import chisel3._
import chisel3.util.Decoupled
import memory.{AddressGenProducerIO, IJKgeneratorBundle, IJKgeneratorConsumerIO, ReadQueueBundle}

class ThreadMemoryAccessIO extends Bundle {
  /** Instruction bits from Thread module */
  val instr = Input(UInt(32.W))
  /** Current state of parent Thread module */
  val threadState = Input(ThreadState())
  /** Indices used when performing .vec operations that go straight to address generator */
  val vec = Decoupled(new AddressGenProducerIO)
  /** Values used when performing .dof operations that go through the EDOF generator */
  val edof = Decoupled(new IJKgeneratorConsumerIO)
  /** Values used when performing .elem, .fcn, .edn1, .edn2 and .sel operations that go to neighbour generator */
  val neighbour = Decoupled(new IJKgeneratorConsumerIO)
  /** Output connections to read queue in memory stage */
  val readQueue = Decoupled(new ReadQueueBundle)
  /** I,J,K values passed to other thread */
  val ijkOut = Output(new IJKgeneratorBundle)
  /** I,J,K values input from other thread */
  val ijkIn = Flipped(new IJKgeneratorBundle)
}

/**
 * This is the main module used for interfacing between a thread and the memory stage.
 * Contains the submodules for generating IJK values and indices for VEC lookups
 */
class ThreadMemoryAccess extends Module {
  val io = IO(new ThreadMemoryAccessIO)

  val resetGenerators = WireDefault(false.B)

  //Declaring both modules with additional reset signal to make our lives easier
  val ijk = withReset(resetGenerators | reset.asBool()) {Module(new IJKGeneratorFSM)}

  //TODO implement VEC access module and hook both up in this module
  // then hook that up inside Threads!
}

package memory

import chisel3._
import chisel3.util._
import pipeline.{RegisterBundle, StypeBaseAddress, StypeLoadStore, StypeMod}
import utils.Config._
import utils.Fixed._

/**
 * Interface between an IJK generator inside a Thread and a consumer module located before the memory stage
 * Instantiate as-is in the IJK-generator/thread, use Flipped() in consumer module
 */
class IJKgeneratorConsumerIO extends Bundle {
  /** Element index bundle */
  val ijk = Output(new IJKBundle)
  /** The base address for this operation */
  val baseAddr = Output(StypeBaseAddress())
  /** Flag indicating whether this set of ijk-value should be seen as padding. Only used in [[EdofGenerator]] */
  val pad = Output(Bool())
  /** S-type modifier of current operation. Only used in [[NeighbourGenerator]] */
  val mod = Output(StypeMod())
}

/**
 * Interface between the [[NeighbourGenerator]] and the [[IndexGenerator]]
 * Instantiate as-is in the neighbour generator, use Flipped() in the index generator
 */
class NeighbourGenIndexGenIO extends Bundle {
  val NUM_PORTS = 3
  /** Vector of ijk-values for which the global indices should be generated */
  val ijk = Output(Vec(NUM_PORTS, new IJKBundle)) //TODO just 3 outputs
  /** Valid flags indicating whether these indices should be operated on */
  val validIjk = Output(Vec(NUM_PORTS, Bool()))
  /** Encoded base address to be read/written to */
  val baseAddr = Output(StypeBaseAddress())
}

/**
 * Interface between the address generator and some other module which generates indices.
 * Use as-is in the producing module, use Flipped() in the address generator
 */
class AddressGenProducerIO extends Bundle {
  /** The encoded base address of the vector to operate on */
  val baseAddr = Output(StypeBaseAddress())
  /** The indices into this vector that should be read/written */
  val indices = Output(Vec(NUM_MEMORY_BANKS, UInt(log2Ceil(NDOF+1).W)))
  /** Valid bits indicating which of the given indices should actually be read/written */
  val validIndices = Output(Vec(NUM_MEMORY_BANKS, Bool()))
}

/**
 * Interface between address generator and memory module.
 * Use as-is in address generator, use Flipped() in memory module.
 * Should always be used with Decoupled() to wrap in ready/valid-signalling
 */
class AddressGenMemoryIO extends Bundle {
  /** Addresses to read/write from/to  */
  val addr = Output(Vec(NUM_MEMORY_BANKS, UInt(MEM_ADDR_WIDTH.W)))
  /** Valid bits indicating whether the operation should be performed or not */
  val validAddress = Output(Vec(NUM_MEMORY_BANKS, Bool()))
}

/**
 * Interface between the memory module and the memory writeback module.
 * Instantiate as-is in the memory module, use Flipped() in the memory writeback module
 */
class MemoryWritebackIO extends Bundle {
  /** Data read from memory / to be written into register file */
  val rdData = Output(Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W)))
}

/** A bundle for grouping the values in the read queue inside of the [[MemoryStage]].
 * Accessed by [[MemoryWriteback]] when writing values back into the register file */
class ReadQueueBundle extends Bundle {
  /** Destination register of the incoming read operation */
  val rd = new RegisterBundle
  /** Global element index modulo 8. Used to access correct indices when executing ELEM, SEL, FCN and EDN1/2 operations */
  val index = UInt(3.W)
  /** S-type modifier of the load operation being performed, for controlling the internal state machine */
  val mod = StypeMod()
}

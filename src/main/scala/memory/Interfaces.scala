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
  /** Flag indicating whether this set of ijk-value should be seen as padding. If true, the validIndices flag is set low on all outputs */
  val pad = Output(Bool())
  /** The base address for this operation */
  val baseAddr = Output(StypeBaseAddress())
  /** Load/store flag */
  val ls = Output(StypeLoadStore())
  /** S-type modifier of current operation. Only used in [[NeighbourGenerator]] */
  val mod = Output(StypeMod())
}

/**
 * Interface between a module producing inputs for the Index Generator, and the index generator itself.
 * Instantiate as-is in the producer module, use Flipped() in the index generator
 */
class IndexGeneratorProducerIO extends Bundle {
  /** Vector of ijk-values for which the global indices should be generated */
  val ijk = Output(Vec(NUM_MEMORY_BANKS, new IJKBundle))
  /** Valid flags indicating whether a load/store operation should be performed from these indices */
  val validIjk = Output(Vec(NUM_MEMORY_BANKS, Bool()))
  /** Encoded base address to be read/written to */
  val baseAddr = Output(StypeBaseAddress())
  /** Load/store flag */
  val ls = Output(StypeLoadStore())
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
  /** Flag indicating whether the operation to perform is a load or store operation */
  val ls = Output(StypeLoadStore())
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
  /** Write enable flag. If (1) a write is performed when valid, if (0) reads are performed */
  val we = Output(Bool())
}



/**
 * Interface between the memory module and the memory writeback module.
 * Instantiate as-is in the memory module, use Flipped() in the memory writeback module
 */
class MemoryWritebackIO extends Bundle {
  /** Data read from memory / to be written into register file */
  val rdData = Output(Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W)))
}

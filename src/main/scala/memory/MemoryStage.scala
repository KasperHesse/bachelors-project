package memory

import chisel3._
import chisel3.util.Decoupled
import pipeline.{IdMemIO, WbIdIO}

/**
 * I/O ports for the [[MemoryStage]] module.
 */
class MemoryStageIO extends Bundle {
  /** Input connections from decode stage */
  val id = Flipped(Decoupled(new IdMemIO))
  /** Output connections to decode stage */
  val wb = new WbIdIO
}

/**
 * A wrapper module around the entirety of the memory stage. Connects to the [[pipeline.Decode]] stage for reads and writes.
 * Implements [[MemoryStageIO]]
 *
 * @param wordsPerBank The number of data words to store in *each* memory bank. The total amount of memory allotted
 *                     is NUM_MEMORY_BANKS*wordsPerBank, each of which is [[utils.Fixed.FIXED_WIDTH]] bits wide.
 * @param memInitFileLocation Location of memory initialization file. Each file must be named 'membank_x.txt', where 'x'
 *                            is a number [0;NUM_MEMORY_BANKS[. The parameter is the relative path to these files, to which the filename is appended.
 *                            If eg memInitFileLocation = "resources/meminit/", one file would be "resources/meminit/membank_0.txt"
 */
class MemoryStage(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new MemoryStageIO)

  val addrGen = Module(new AddressGenerator())
  val mem = Module(new OnChipMemory(wordsPerBank, memInitFileLocation))

  addrGen.io.in.bits <> io.id.bits.vec
  mem.io.addrGen <> addrGen.io.mem
}

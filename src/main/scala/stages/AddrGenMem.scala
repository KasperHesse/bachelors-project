package memory.substages

import chisel3._
import chisel3.util.Decoupled
import memory._

/**
 * A wrapper module for testing only the address generator and the memory stage, without any other modules interfering
 * @param wordsPerBank The number of data words in each memory bank
 * @param memInitFileLocation The relative path of memory initialization files. See [[OnChipMemory]] for more info.
 */
class AddrGenMem(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new AddressGenProducerIO))
    val out = Decoupled(new MemoryWritebackIO)
  })

  val addrGen = Module(new AddressGenerator())
  val mem = Module(new OnChipMemory(wordsPerBank, memInitFileLocation))

  addrGen.io.in <> io.in
  mem.io.addrGen <> addrGen.io.mem
  io.out <> mem.io.wb

  mem.io.writeQueue := DontCare
}

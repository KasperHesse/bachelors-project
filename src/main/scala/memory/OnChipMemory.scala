package memory

import chisel3._
import utils.Fixed._

/**
 * A class representing the memory accessible directly on-board the FPGA. See [[MemoryIO]] for IO details.
 * @param numBanks The number of memory banks to instantiate, and thus the number of values that can be read/write during
 *                 one clock cycle.
 * @param wordsPerBank The number of data words to store in *each* memory bank. The total amount of memory allotted
 *                     is numBanks*wordsPerBank, each of which is [[utils.Fixed.FIXED_WIDTH]] bits wide.
 */
class OnChipMemory(val numBanks: Int, val wordsPerBank: Int) extends Module {
  val io = IO(new MemoryIO(numBanks))

  //8 memory banks holding 1000 elements each
  //Cannot store memory in vecs. Must store memory in indexedSeq and use vecs to access the elements
  val memb = for(i <- 0 until numBanks) yield {
    SyncReadMem(wordsPerBank, SInt(FIXED_WIDTH.W))
  }
  val rdData = Wire(Vec(numBanks, SInt(FIXED_WIDTH.W)))


  for(i <- 0 until numBanks) {
    rdData(i) := DontCare
    when(io.in.en) {
      rdData(i) := memb(i).read(io.in.rdAddr(i))
      when(io.in.we) {
        memb(i)(io.in.wrAddr(i)) := io.in.wrData(i)
      }
    }
  }
  io.out.rdData := rdData

}

/**
 * I/O ports for the on-chip memory.
 * @param numBanks The number of memory banks to instantiate
 */
class MemoryIO(val numBanks: Int) extends Bundle {
  val in = Input(new MemoryInput(numBanks))
  val out = Output(new MemoryOutput(numBanks))

  /**
   * Input ports for the on-chip memory interface
   * @param numBanks The number of memory banks to instantiate
   */
  class MemoryInput(val numBanks: Int) extends Bundle {
    /** Read addresses. One word is read from each of these addresses on the next clock cycle */
    val rdAddr = Vec(numBanks, UInt(32.W))
    /** Write addresses. The data in wrData is written to these addresses if we is high */
    val wrAddr = Vec(numBanks, UInt(32.W))
    /** Chip enable */
    val en = Bool()
    /** Write enable */
    val we = Bool()
    /** Write data */
    val wrData = Vec(numBanks, SInt(FIXED_WIDTH.W))
  }

  /**
   * Output ports for the on-chip memory interface
   * @param numBanks The number of memory banks to instantiate
   */
  class MemoryOutput(val numBanks: Int) extends Bundle {
    /** Data read from memory */
    val rdData = Vec(numBanks, SInt(FIXED_WIDTH.W))
  }
}
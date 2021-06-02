package memory

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util._
import utils.Assembler.writeMemInitFile
import utils.Fixed._
import utils.Config._

import scala.io.Source


/**
 * I/O ports for the memory wrapper.
 */
class OnChipMemoryIO extends Bundle {
  /** Connections to the address generator */
  val addrGen = Flipped(Decoupled(new AddressGenMemoryIO))
  /** Connections to memory writeback module */
  val wb = Decoupled(new MemoryWritebackIO)
  /** Connections to memory write queue */
  val writeQueue = Flipped(Decoupled(new WriteQueueBundle))
  /** Write enable flag. If (1) a write is performed when valid, if (0) reads are performed */
  val we = Input(Bool())
}

/**
 * A class representing the memory accessible directly on-board the FPGA. See [[OnChipMemoryIO]] for IO details.
 *
 * @param wordsPerBank The number of data words to store in *each* memory bank. The total amount of memory allotted
 *                     is NUM_MEMORY_BANKS*wordsPerBank, each of which is [[utils.Fixed.FIXED_WIDTH]] bits wide.
 * @param memInitFileLocation Location of memory initialization file. Each file must be named 'membank_x.txt', where 'x'
 *                            is a number [0;NUM_MEMORY_BANKS[. The parameter is the relative path to these files, to which the filename is appended.
 *                            If eg memInitFileLocation = "resources/meminit/", one file would be "resources/meminit/membank_0.txt"
 */
class OnChipMemory(val wordsPerBank: Int, val memInitFileLocation: String = "src/test/scala/memory/membankinit/") extends Module {
  require(isPow2(NUM_MEMORY_BANKS), "Number of memory banks must be a power of 2")
  val io = IO(new OnChipMemoryIO)

  // --- MODULES ---
  /** Memory banks holding N elements each  */
  val membank = for(i <- 0 until NUM_MEMORY_BANKS) yield {
    SyncReadMem(wordsPerBank, SInt(FIXED_WIDTH.W))
  }

  // -- SIGNALS AND WIRES ---
  /** Data read from memory banks */
  val rdData = Wire(Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W)))
  /** Asserted when the producer has valid data and we are ready to operate on it */
  val validOp = io.wb.ready && io.addrGen.valid
  /** Wire containing write-enable signals for each memory bank */
  val we = Wire(Vec(NUM_MEMORY_BANKS, Bool()))

  // -- LOGIC ---
  //Addresses are right-shifted by log2Ceil(NUM_MEMORY_BANKS) such that address x000, x001, x010 ... x111
  // are all represented as address 'x' in their respective banks. The lower 3 bits are thus used to select the relevant banks
  //TODO When performing st.sel, we should move the incoming value at wrData(0) to the correct index, based on ijk iteration value
  for(i <- membank.indices) {
    //Set up read accessors. If address it not valid, we replace the read data with 0's
    rdData(i) := membank(i).read((io.addrGen.bits.addr(i) >> log2Ceil(NUM_MEMORY_BANKS)).asUInt(), validOp)

    //Set up write accessors. Only write if address is valid
    we(i) := io.we && io.addrGen.bits.validAddress(i) && io.addrGen.valid
    when(we(i)) {
      val wrAddr = (io.addrGen.bits.addr(i) >> log2Ceil(NUM_MEMORY_BANKS)).asUInt()
      val wrData = io.writeQueue.bits.wrData(i)
      membank(i).write(wrAddr, wrData)
    }
  }

  // --- OUTPUT CONNECTIONS ---
  //Output data is valid on the clock cycle after receiving a valid input
  //Only toggle reads/writes when we are ready and input is valid
  //We are ready whenever the writeback builder signals ready
  io.addrGen.ready := io.wb.ready
  io.writeQueue.ready := io.we && io.addrGen.valid
  io.wb.valid := RegNext(validOp) && !RegNext(io.we)

  for(i <- 0 until NUM_MEMORY_BANKS) {
    io.wb.bits.rdData(i) := Mux(RegNext(io.addrGen.bits.validAddress(i)), rdData(i), 0.S)
  }


  //Set up memory contents
  if(memInitFileLocation.nonEmpty) {
    for (i <- 0 until NUM_MEMORY_BANKS) {
      val file = if (memInitFileLocation.takeRight(1).equals("/")) {
        memInitFileLocation + "membank_" + i + ".txt"
      } else {
        memInitFileLocation + "/membank_" + i + ".txt"
      }
      if(SIMULATION) {
        initMemBanks(wordsPerBank, memInitFileLocation)
        loadMemoryFromFile(membank(i), file)
      } else {
        loadMemoryFromFileInline(membank(i), file)
      }
    }
  }


  //NOTE: SyncReadMem does not keep the read value until the next read is issued.
  //If a write is performed to the previously read value, the read data will also update
  //To fix this, a register is necessary to sample the output data whenever validOp is asserted
  //See UsingSyncReadMem for implementation ideas.


  def initMemBanks(wordsPerBank: Int, memInitFileLocation: String): Unit = {
    for(bank <- 0 until NUM_MEMORY_BANKS) {
      val file = if (memInitFileLocation.takeRight(1).equals("/")) {
        memInitFileLocation + "membank_" + bank + ".txt"
      } else {
        memInitFileLocation + "/membank_" + bank + ".txt"
      }
      val values = for(index <- 0 until wordsPerBank) yield {
        double2fixed(index*NUM_MEMORY_BANKS+bank)
      }
      writeMemInitFile(file, values.toArray)
    }
  }
}
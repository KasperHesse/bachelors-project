package execution

import chisel3._
import chisel3.util._
import chisel3.util.experimental.{loadMemoryFromFile, loadMemoryFromFileInline}
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import utils.Fixed.double2fixed

import java.io.{BufferedWriter, FileWriter}

/**
 * A scalar register file. Register at index 0 is always tied to the value 0. Implements [[ScalarRegFileIO]].
 */
class ScalarRegisterFile(memInitFileLocation: String) extends Module {
  val io = IO(new ScalarRegFileIO)

  val arr: Array[SInt] = Array.fill[SInt](NUM_SREG)(0.S(FIXED_WIDTH.W))
  if(SIMULATION) {
    for(i <- 0 until NUM_SREG) {
      arr(i) = double2fixed(i).S(FIXED_WIDTH.W)
    }
  }

  //Trying to map it down to syncreadmem
  val mem = SyncReadMem(NUM_SREG, SInt(FIXED_WIDTH.W))
  when(io.we && io.rd =/= 0.U) {
    mem.write(io.rd, io.wrData)
  }
  io.rdData1 := mem.read(io.rs1)
  io.rdData2 := mem.read(io.rs2)

  //Create meminit file
  val writer = new BufferedWriter(new FileWriter(memInitFileLocation))
  for(v <- arr) {
    v.litValue.toByteArray.foreach(b => writer.write(f"$b%02x"))
    writer.write("\n")
  }
  writer.close()

  if(INLINE) {
    loadMemoryFromFileInline(mem, memInitFileLocation)
  } else {
    loadMemoryFromFile(mem, memInitFileLocation)
  }
}

class ScalarRegFileIO extends Bundle {
  /** Write enable bit */
  val we = Input(Bool())
  /** Register select 1 */
  val rs1 = Input(UInt(log2Ceil(NUM_SREG).W))
  /** Register select 2 */
  val rs2 = Input(UInt(log2Ceil(NUM_SREG).W))
  /** Destination register for write */
  val rd = Input(UInt(log2Ceil(NUM_SREG).W))
  /** Write data */
  val wrData = Input(SInt(FIXED_WIDTH.W))
  /** Read data 1, read from register specified by rs1 */
  val rdData1 = Output(SInt(FIXED_WIDTH.W))
  /** Read data 2, read from register specified by rs2 */
  val rdData2 = Output(SInt(FIXED_WIDTH.W))
}

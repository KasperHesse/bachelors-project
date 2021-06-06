package pipeline

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util.experimental.loadMemoryFromFileInline
import chisel3.util.log2Ceil
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import utils.Fixed.double2fixed

import java.io.{BufferedWriter, FileWriter}

/**
 * A module representing a vector register file. This module generates the hardware "inline" (ie. not inside of an explicit module), to avoid some weird compilation issues in Quartus,
 * that arose when the [[VectorRegisterFile]] was used.
 * @param width The number of entries in the vector register file. If eg. width=32, then mem.length == 32
 * @param depth The number of values in each entry. If eg depth=16, then mem(0).length == 16
 * @param memInitFileLocation Location of a memory initialization file, if any. If this is given, the loadMemoryFromFile / loadMemoryFromFileInline annotation is used to generate a verilog $readmemh statement.
 *                            If [[utils.Config.SIMULATION]] is true, the non-inline version is used. false, the inlined version if used.
 */
class InlineVectorRegisterFile(width: Int, depth: Int, memInitFileLocation: String) {
  val mem = SyncReadMem(width, SInt((depth*FIXED_WIDTH).W))
  val arr: Array[Array[SInt]] = Array.ofDim[SInt](width,depth) //TODO set this to match contents given at compile time

  /**
   * Creates a read port into the vector register file
   * @param rs Register select for this read port
   * @return A handle to the read data.
   */
  def setReadPort(rs: UInt): Vec[SInt] = {
    val rdData = mem(rs)
    val rdDataVec = Wire(Vec(depth, SInt(FIXED_WIDTH.W)))
    for(i <- 0 until depth) {
      rdDataVec(i) := rdData((i+1)*FIXED_WIDTH-1, i*FIXED_WIDTH).asSInt()
    }
    rdDataVec
  }

  /**
   * Creates a write port into the vector register file.
   * @param rd The destination register for the write
   * @param wrData The data to be written
   * @param we Write enable flag
   */
  def setWritePort(rd: UInt, wrData: Vec[SInt], we: Bool): Unit = {
    when(we) {
      mem.write(rd, wrData.asUInt().asSInt())
    }
  }

  /**
   * Initializes the memory file given when creating the vector register file.
   * Initialises the file such that mem(0)(0) = fixed(0), mem(0)(1) = fixed(1), ... mem(0)(depth-1)=fixed(depth-1), mem(1)(0)=fixed(depth), mem(2)(0)=fixed(2*depth).
   * In general, mem(w)(d)=fixed(w*depth+d)
   * Will also initalilize the [[arr]] field in this object to match the values in memory.
   */
  def initMemory(): Unit = {
    //Create memory file
    //Each entry holds 'depth' values one after the other
    val memArray = Array.fill[BigInt](width)(0)
    for(w <- 0 until width) {
      for(d <- 0 until depth) {
        val v = double2fixed(w*depth+d)
        memArray(w) |= BigInt(v) << (d*FIXED_WIDTH)
        arr(w)(d) = v.S(FIXED_WIDTH.W)
      }
    }

    val writer = new BufferedWriter(new FileWriter(memInitFileLocation))
    for(m <- memArray) {
      m.toByteArray.foreach(b => writer.write(f"$b%02x"))
      writer.write("\n")
    }
    writer.close()
  }

  if(SIMULATION) {
    loadMemoryFromFile(mem, memInitFileLocation)
//  } else {
//    loadMemoryFromFileInline(mem, memInitFileLocation)
  }

}

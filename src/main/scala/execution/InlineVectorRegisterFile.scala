package execution

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
 * @param width The number of entries in the vector register file.
 * @param depth The number of values in each entry.
 * @param memInitFileLocation Location of a memory initialization file, if any. If this is given, the loadMemoryFromFile / loadMemoryFromFileInline annotation is used to generate a verilog $readmemh statement.
 *                            If [[utils.Config.SIMULATION]] is true, the non-inline version is used. false, the inlined version if used.
 */
class InlineVectorRegisterFile(width: Int, depth: Int, memInitFileLocation: String) {
  val arr: Array[Array[SInt]] = Array.fill[SInt](width, depth)(0.S(FIXED_WIDTH.W))

  //Creating 'depth' arrays, each of which holds 'width' SInts. When performing a read/write to an address,
  //the n'th value of each of the 'depth' memories is output. This removes the need for any switching logic
  val mem = Array.fill(depth)(SyncReadMem(width, SInt(FIXED_WIDTH.W)))

  /**
   * Creates a read port into the vector register file
   * @param rs Register select for this read port
   * @return A handle to the read data.
   */
  def setReadPort(rs: UInt): Vec[SInt] = {
    val rdData = Wire(Vec(depth, SInt(FIXED_WIDTH.W)))
    for(i <- 0 until depth) {
      rdData(i) := mem(i).read(rs+0.U) //Things break if we don't add 0.U ...
    }
    rdData
  }

  /**
   * Creates a write port into the vector register file.
   * @param rd The destination register for the write
   * @param wrData The data to be written
   * @param we Write enable flag
   */
  def setWritePort(rd: UInt, wrData: Vec[SInt], we: Bool): Unit = {
    for(i <- 0 until depth) {
      when(we) {
        mem(i).write(rd, wrData(i))
      }
    }
  }

  /**
   * Initializes the memory file given when creating the vector register file.
   * Initialises the file such that mem(0)(0) = fixed(0), mem(0)(1) = fixed(1), ... mem(0)(depth-1)=fixed(depth-1), mem(1)(0)=fixed(depth), mem(2)(0)=fixed(2*depth).
   * In general, mem(w)(d)=fixed(w*depth+d)
   * Will also initalilize the [[arr]] field in this object to match the values in memory.
   */
  def initMemory(): Unit = {
    //Create memory inits
    //2D array, width*depth
    val memArray = Array.ofDim[BigInt](depth, width)
    for(d <- 0 until depth) {
      for(w <- 0 until width) {
        val v = double2fixed(w*depth+d)
        arr(w)(d) = v.S(FIXED_WIDTH.W)
        memArray(d)(w) = BigInt(v)
      }
    }

    for(i <- 0 until depth) {
      val mif = s"${memInitFileLocation}_$i.hex.txt"
      val writer = new BufferedWriter(new FileWriter(mif))
      for(v <- memArray(i)) {
        v.toByteArray.foreach(b => writer.write(f"$b%02x"))
        writer.write("\n")
      }
      writer.close()
    }

    if(INLINE) {
      for (d <- 0 until depth) {
        loadMemoryFromFileInline(mem(d), s"${memInitFileLocation}_$d.hex.txt")
      }
    } else {
      for(d <- 0 until depth) {
        loadMemoryFromFile(mem(d), s"${memInitFileLocation}_$d.hex.txt")
      }
    }
  }



}

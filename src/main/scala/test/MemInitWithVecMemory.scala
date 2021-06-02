package test

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util.experimental.loadMemoryFromFileInline

import java.io.{BufferedWriter, FileWriter}

/**
 * A module to experiment with a workaround, since the Chisel loadMemoryFromFile annotation does not support vector-type memories
 * This just utilizes a very wide word, bitvectors of which are then extracted to give the illusion of a vector memory.
 */
class MemInitWithVecMemory(width: Int, depth: Int, bits: Int) extends Module {
  val io = IO(new Bundle {
    val rdAddr = Input(UInt(bits.W))
    val rdData = Output(Vec(depth, SInt(bits.W)))
    val wrAddr = Input(UInt(bits.W))
    val wrData = Input(Vec(depth, SInt(bits.W)))
    val we = Input(Bool())
  })

  val mem = SyncReadMem(width, SInt((depth*bits).W))
  val rdData = mem.read(io.rdAddr)
  val wrData = io.wrData.asUInt().asSInt() //asUInt to cast down from vec, then asSInt to cast to correct data type
  for(i <- 0 until depth) {
    io.rdData(i) := rdData((i+1)*bits-1, i*bits).asSInt()
  }
  when(io.we) {
    mem.write(io.wrAddr, wrData)
  }

  //Create memory file
  //Each entry holds 'depth' values
  val memArray = Array.fill[BigInt](width)(0)
  for(w <- 0 until width) {
    for(d <- 0 until depth) {
      memArray(w) |= BigInt(w*depth+d) << (d*bits)
    }
  }
  val memfile = "meminit.txt"
  val writer = new BufferedWriter(new FileWriter(memfile))
  for(m <- memArray) {
    m.toByteArray.foreach(b => writer.write(f"$b%02x"))
//    writer.write(s"${m.toString(16)}\n")
    writer.write("\n")
  }
  writer.close()
  loadMemoryFromFile(mem, "meminit.txt")
}

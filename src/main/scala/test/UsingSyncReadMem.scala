package test

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util._

class UsingSyncReadMem extends Module {
  val io = IO(new Bundle {
    val we = Input(Vec(2, Bool()))
    val re = Input(Bool())
    val en = Input(Bool())
    val addr = Input(Vec(2, UInt(8.W)))
    val wrData = Input(Vec(2, UInt(16.W)))
    val rdData = Output(Vec(2, UInt(16.W)))
  })

  val mem = for(i <- 0 until 2) yield {
    SyncReadMem(256, UInt(16.W))
  }
  val we = Wire(Vec(2, Bool()))
  val validOp = io.re && io.en

  val rdData = Wire(Vec(2, UInt(16.W)))
  val rdDataReg = RegEnable(rdData, validOp)

  for(i <- 0 until 2) {
    rdData(i) := mem(i).read((io.addr(i) >> 2).asUInt)
    we(i) := io.we(i) && io.en
    when(we(i)) {
      mem(i).write((io.addr(i) >> 2).asUInt, io.wrData(i))
    }
    loadMemoryFromFile(mem(i), "src/test/scala/memory/membankinit/membank_" + i + ".txt")
  }

  io.rdData := rdDataReg
}

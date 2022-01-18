package arithmetic

import chisel3._
import chisel3.util.{Cat, switch, is}

class LeadingZerosCounter64Bit extends Module {
  val io = IO(new Bundle {
    val in = Input(UInt(64.W))
    val cnt = Output(UInt(6.W))
    val Q = Output(Bool())
  })

  val LZC = for(i <- 0 until 2) yield {
    val lzc = Module(new LeadingZerosCounter32Bit)
    lzc
  }
  LZC(1).io.in := io.in(63,32)
  LZC(0).io.in := io.in(31,0)
  io.Q := LZC(1).io.Q & LZC(0).io.Q
  io.cnt := Mux(LZC(1).io.Q, Cat(1.U, LZC(0).io.cnt), Cat(0.U, LZC(1).io.cnt))
}
/**
 * Counts the number of leading zeros in a 32-bit value. Based on the design presented by Milenkovic, Stankovic & Milic, 2015
 */
class LeadingZerosCounter32Bit extends Module {
  val io = IO(new Bundle {
    val in = Input(Bits(32.W))
    val cnt = Output(UInt(5.W))
    val Q = Output(Bool())
  })

  val NLC = for (i <- 0 until 8) yield {
    val nlc = Module(new NibbleLocalCount)
    nlc
  }
  val BNE = Module(new BoundaryNibbleEncoder)

  for (i <- 0 until 8) {
    NLC(i).io.X := io.in(i*4+3, i*4)
  }
  BNE.io.a := Cat(NLC(0).io.a, Cat(NLC(1).io.a, Cat(NLC(2).io.a, Cat(NLC(3).io.a, Cat(NLC(4).io.a, Cat(NLC(5).io.a, Cat(NLC(6).io.a, NLC(7).io.a)))))))

  val muxO = Wire(UInt(2.W))
  muxO := NLC(7).io.Z
  switch(BNE.io.y) {
    is(1.U) {muxO := NLC(6).io.Z}
    is(2.U) {muxO := NLC(5).io.Z}
    is(3.U) {muxO := NLC(4).io.Z}
    is(4.U) {muxO := NLC(3).io.Z}
    is(5.U) {muxO := NLC(2).io.Z}
    is(6.U) {muxO := NLC(1).io.Z}
    is(7.U) {muxO := NLC(0).io.Z}
  }
  io.cnt := Cat(BNE.io.y, muxO)
  io.Q := BNE.io.Q
}

class BoundaryNibbleEncoder extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(8.W))
    val y = Output(UInt(3.W))
    val Q = Output(Bool())
  })
  val a = io.a

  io.Q := io.a.andR()
  val y2 = io.a(3,0).andR()
  val y1 = a(0) & a(1) & (!a(2) | !a(3) | a(4) & a(5))
  val y0 = a(0) & (!a(1) | a(2) & !a(3)) | (a(0) & a(2) & a(4) & (!a(5) | a(6)))
  io.y := Cat(y2, Cat(y1, y0))
}

class NibbleLocalCount extends Module {
  val io = IO(new Bundle {
    val X = Input(UInt(4.W))
    val a = Output(Bool())
    val Z = Output(UInt(2.W))
  })

  io.a := !io.X.orR()
  val Z1 = !(io.X(3) | io.X(2))
  val Z0 = !(io.X(3) | (io.X(1) & !io.X(2)))
  io.Z := Cat(Z1,Z0)
}

object LZC extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new LeadingZerosCounter32Bit())
}
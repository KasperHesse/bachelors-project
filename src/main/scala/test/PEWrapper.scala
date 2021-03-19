package test

import chisel3._
class PEWrapper extends Module {
  val io = IO(new Bundle {
    val o = Output(Bool())
    val d = Output(Bool())
  })

  val PE = Module(new vector.ProcessingElement)

  val cntReg = RegInit(0.S(54.W))
  cntReg := Mux(cntReg === 100.S, 0.S, cntReg + 1.S)

  PE.io.in.a := cntReg
  PE.io.in.b := cntReg
  PE.io.in.valid := cntReg(0)
  PE.io.in.op := cntReg(4,0)
  PE.io.in.macLimit := 2.U

  io.o := PE.io.out.res.asBools().reduce((a,b) => (a & b))
  io.d := PE.io.out.valid
}

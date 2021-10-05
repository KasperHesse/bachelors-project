package test

import chisel3._
import chisel3.util._

class InSystemMemoryContent(clkFreq: Int = 50000000) extends Module {
  val io = IO(new Bundle {
    val out = Output(UInt(8.W))
  })

  val mem = SyncReadMem(8, UInt(8.W)) //Memory

  val pc = RegInit(0.U(3.W)) //Program counter

  val timerReg = RegInit(0.U(log2Ceil(clkFreq+1).W)) //Timer for updating system
  val cntReg = RegInit(0.U(8.W)) //Counter to set/get values

  val rdData = RegInit(0.U(8.W))


  //Update counter once every second
  timerReg := Mux(timerReg === (clkFreq-1).U, 0.U, timerReg + 1.U)

  when(timerReg === 0.U) {
    rdData := mem.read(pc) //Read on the bottom
  } .elsewhen(timerReg === ((clkFreq-1)/2).U) {
    mem.write(pc, cntReg) //Write value on the middle
  } .elsewhen(timerReg === ((clkFreq-1)*3/4).U) {
    pc := pc + 1.U //Update pc and cntreg before next read
    cntReg := cntReg + 1.U
  }

  io.out := rdData
}

object InSystemMemoryContent extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new InSystemMemoryContent(50000000), Array("--target-dir", "generated"))
}
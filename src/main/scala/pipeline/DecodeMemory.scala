package pipeline

import chisel3._
import memory.MemoryStage

class DecodeMemory(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new IfIdIO)
    val ex = new IdExIO
    val memWb = new WbIdIO
  })

  val decode = Module(new Decode)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)

  decode.io.fe <> io.in
  decode.io.mem <> mem.io.id
  io.memWb <> mem.io.wb
  decode.io.memWb <> mem.io.wb
  io.ex <> decode.io.ex

  control.io.id <> decode.io.ctrl
  control.io.mem <> mem.io.ctrl
  control.io.fe <> DontCare
  control.io.fe.instr := io.in.instr
  control.io.ex <> DontCare

  decode.io.wb <> DontCare
  decode.io.wb.we := false.B

}

package pipeline

import chisel3._
import memory.MemoryStage

class DecodeMemory(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new IfIdIO)
    val memWb = new WbIdIO
  })

  val decode = Module(new Decode)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)

  decode.io.fe <> io.in
  decode.io.mem <> mem.io.id
  io.memWb <> mem.io.wb
  //TODO finish these connections, try to execute load instructions via the decoder

  control.io.id <> decode.io.ctrl
  control.io.fe <> DontCare
  control.io.fe.instr := io.in.instr
  control.io.ex <> DontCare


}

package stages

import chisel3._
import execution._
import memory.MemoryStage

class DecodeMemory(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new Bundle {
    val in = Flipped(new IfIdIO)
    val ex = new IdExIO
    val memWb = new WbIdIO
    val idStateUint = Output(UInt(4.W))
  })

  val decode = Module(new Decode)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)

  io.idStateUint := decode.io.ctrl.stateUint

  decode.io.fe <> io.in
  decode.io.mem <> mem.io.id
  io.memWb <> mem.io.wb
  decode.io.memWb <> mem.io.wb
  io.ex <> decode.io.ex

  control.io.id <> decode.io.ctrl
  control.io.mem <> mem.io.ctrl
  control.io.fe <> DontCare
  control.io.ex <> DontCare
  control.io.ex.empty := true.B
  control.io.ex.macEmpty := true.B

  decode.io.wb <> DontCare
  decode.io.wb.we := false.B



}

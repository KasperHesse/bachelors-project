package stages

import chisel3._
import execution._

/**
 * A module encompassing all modules present in the execution logic.
 * @param memfile Path to a memory file for the instruction memory, used when simulating
 */
class ExecutePipeline(memfile: String = "") extends Module {
  val io = IO(new Bundle {
    val wbout = new WbIdIO
    val idctrl = new IdControlIO
    val fectrl = new IfControlIO
    val idout = new IdExIO
  })

  val fetch = Module(new Fetch(memfile=memfile))
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val writeback = Module(new Writeback)
  val forward = Module(new Forwarding)
  val control = Module(new Control)

  io.wbout <> writeback.io.id
  io.idctrl <> decode.io.ctrl
  io.fectrl <> fetch.io.ctrl
  io.idout <> decode.io.ex

  fetch.io.id <> decode.io.fe
  decode.io.ex <> execute.io.id
  execute.io.wb <> writeback.io.ex
  writeback.io.id <> decode.io.wb
  forward.io.ex <> execute.io.fwd
  forward.io.wb <> writeback.io.fwd

  control.io.fe <> fetch.io.ctrl
  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl

  decode.io.mem <> DontCare
  decode.io.memWb <> DontCare
  control.io.mem.wqCount := 0.U
  control.io.mem.rqCount := 0.U
}

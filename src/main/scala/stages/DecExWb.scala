package stages

import chisel3._
import execution._

/**
 * A module encompassing the decode, execute and writeback stages + a control stage. Used for testing
 */
class DecExWb extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new IfIdIO)
    val idctrl = new IdControlIO
    val exctrl = new ExControlIO
    val wb = new WbIdIO
  })

  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val wb = Module(new Writeback)
  val fwd = Module(new Forwarding)
  val control = Module(new Control)

  //Connect outputs to top level
  io.idctrl <> decode.io.ctrl
  io.exctrl <> execute.io.ctrl

  io.in <> decode.io.fe
  decode.io.ex <> execute.io.id
  execute.io.wb <> wb.io.ex
  wb.io.id <> decode.io.wb
  io.wb := wb.io.id

  fwd.io.wb <> wb.io.fwd
  fwd.io.ex <> execute.io.fwd

  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl

  /** Dontcares */
  decode.io.mem := DontCare
  decode.io.memWb.we := false.B
  decode.io.memWb.wrData := DontCare
  decode.io.memWb.rd := DontCare
  control.io.mem.wqCount := 0.U
  control.io.mem.rqCount := 0.U
}
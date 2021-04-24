package pipeline

import chisel3._

/**
 * A module encompassing the decode, execute and writeback stages + a control stage. Used for testing
 */
class DecExWb extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new IpIdIO)
    val idctrl = new IdControlIO
    val exctrl = new ExControlIO
    val wb = new WbIdIO
  })

  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val wb = Module(new Writeback)
  val fwd = Module(new Forwarding)
  val control = Module(new Control)


  io.in <> decode.io.id
  decode.io.ex <> execute.io.in
  execute.io.out <> wb.io.in
  wb.io.out <> decode.io.wb
  io.wb := wb.io.out

  fwd.io.wb <> wb.io.fwd
  fwd.io.ex <> execute.io.fwd

  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.fe.instr := io.in.instr

  io.idctrl.state := decode.io.ctrl.state
  io.idctrl.execThread := decode.io.ctrl.execThread

  io.exctrl.empty := execute.io.ctrl.empty
  io.exctrl.op := execute.io.ctrl.op
  io.exctrl.queueHead := execute.io.ctrl.queueHead

  for(i <- 0 until 2) {
    io.idctrl.threadCtrl(i).stateUint := decode.io.ctrl.threadCtrl(i).stateUint
    io.idctrl.threadCtrl(i).state := decode.io.ctrl.threadCtrl(i).state
    io.idctrl.threadCtrl(i).finalCycle := decode.io.ctrl.threadCtrl(i).finalCycle
    io.idctrl.threadCtrl(i).firstCycle := decode.io.ctrl.threadCtrl(i).firstCycle
    io.idctrl.threadCtrl(i).op := decode.io.ctrl.threadCtrl(i).op
    io.idctrl.threadCtrl(i).rtypemod := decode.io.ctrl.threadCtrl(i).rtypemod
    io.idctrl.threadCtrl(i).rs1 := decode.io.ctrl.threadCtrl(i).rs1
    io.idctrl.threadCtrl(i).rs2 := decode.io.ctrl.threadCtrl(i).rs2
  }

  /** Dontcares */
  decode.io.mem := DontCare
}
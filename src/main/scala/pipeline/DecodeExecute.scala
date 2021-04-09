package pipeline

import chisel3._

/**
 * A module encompassing the decode and execute stages. Mainly used for testing the two stages
 */
class DecodeExecute extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new IpIdIO)
    val idctrl = new IdControlOldIO
    val exctrl = new ExControlIO
    val out = new ExWbIO
    val idstall = Output(Bool())
  })

  val decode = Module(new DecodeOld)
  val execute = Module(new Execute)
  val control = Module(new Control)

  decode.io.in <> io.in
  execute.io.out <> io.out

  decode.io.ex <> execute.io.in

  decode.io.ctrl <> control.io.id
  execute.io.ctrl <> control.io.ex

  decode.io.ctrl.iload := io.idctrl.iload

  //Push out all signals from decode to top level component
  io.idctrl.finalCycle := decode.io.ctrl.finalCycle
  io.idctrl.firstCycle := decode.io.ctrl.firstCycle
  io.idctrl.state := decode.io.ctrl.state
  io.idctrl.op := decode.io.ctrl.op
  io.idstall := control.io.id.stall
  io.idctrl.rtypemod := decode.io.ctrl.rtypemod

  io.exctrl.count := execute.io.ctrl.count
  io.exctrl.op := execute.io.ctrl.op
}

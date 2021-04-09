package pipeline

import chisel3._

/**
 * A module encompassing the decode and execute stages. Mainly used for testing the two stages
 */
class DecodeExecute extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new IpIdIO)
    val idctrl = new IdControlIO
    val exctrl = new ExControlIO
    val out = new ExWbIO
    val idstall = Output(Bool())
  })

//  val decode = Module(new DecodeOld)
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val control = Module(new Control)

  decode.io.in <> io.in
  execute.io.out <> io.out

  decode.io.ex <> execute.io.in

  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.fe.instr := io.in.instr

//  decode.io.ctrl.iload := io.idctrl.iload
  io.idctrl.state := decode.io.ctrl.state
  io.idstall := control.io.id.stall

  //Push out all signals from decode to top level component
  for(i <- 0 until 2) {
    io.idctrl.threadCtrl(i).finalCycle := decode.io.ctrl.threadCtrl(i).finalCycle
    io.idctrl.threadCtrl(i).firstCycle := decode.io.ctrl.threadCtrl(i).firstCycle
    io.idctrl.threadCtrl(i).op := decode.io.ctrl.threadCtrl(i).op
    io.idctrl.threadCtrl(i).rtypemod := decode.io.ctrl.threadCtrl(i).rtypemod
    io.idctrl.threadCtrl(i).stateUint := decode.io.ctrl.threadCtrl(i).stateUint
    io.idctrl.threadCtrl(i).state := decode.io.ctrl.threadCtrl(i).state
  }
  io.idctrl.execThread := decode.io.ctrl.execThread
  io.idstall := control.io.id.threadCtrl(decode.io.ctrl.execThread).stall

  io.exctrl.count := execute.io.ctrl.count
  io.exctrl.op := execute.io.ctrl.op

  /** DONTCARES */
  decode.io.mem := DontCare
}

package pipeline

import chisel3._

/**
 * A module encompassing the decode and execute stages. Mainly used for testing the two stages together.
 */
class DecodeExecute extends Module {
  val io = IO(new Bundle{
    val in = Flipped(new IfIdIO)
    val idctrl = new IdControlIO
    val exctrl = new ExControlIO
    val out = new ExWbIO
    val execStall = Output(Bool())
  })

  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val control = Module(new Control)

  io.in <> decode.io.fe
  decode.io.ex <> execute.io.in
  execute.io.out <> io.out

  execute.io.fwd := DontCare

  execute.io.fwd.rs1swap := false.B
  execute.io.fwd.rs2swap := false.B

  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.fe.instr := io.in.instr

  //Connect all relevant outputs
  io.idctrl.state := decode.io.ctrl.state
  io.idctrl.execThread := decode.io.ctrl.execThread
  io.exctrl.empty := execute.io.ctrl.empty
  io.exctrl.op := execute.io.ctrl.op
  io.exctrl.queueHead := execute.io.ctrl.queueHead

  io.execStall := control.io.ex.stall

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

  /** DONTCARES */
  decode.io.mem := DontCare
  decode.io.wb := DontCare
}

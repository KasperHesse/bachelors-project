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
  })

  val decode = Module(new Decode)
  val execute = Module(new Execute)

  decode.io.in <> io.in
  decode.io.ctrl <> io.idctrl
  io.idctrl.finalCycle := decode.io.ctrl.finalCycle
  io.idctrl.state := decode.io.ctrl.state
  decode.io.ctrl.exproc := (execute.io.ctrl.count =/= 0.U)
  decode.io.out <> execute.io.in
  execute.io.ctrl <> io.exctrl
  execute.io.out <> io.out
}

package pipeline

import chisel3._

/**
 * A module encompassing the decode and execute stages. Mainly used for testing the two stages
 */
class DecodeExecute extends Module {
  val io = IO(new Bundle{
    val iload = Input(Bool())
    val out = Flipped(new ExWbIO)
  })

  val decode = Module(new Decode)
  val execute = Module(new Execute)

  decode.io.out <> execute.io.in
  decode.io.ctrl.iload := io.iload
  io.out <> execute.io.out
}

package arithmetic

import chisel3._

class Top extends Module {
  val io = IO(new Bundle {
  })

}

object Top extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new Top())
}

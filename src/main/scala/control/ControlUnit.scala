package control

import chisel3._

/**
 * The central control unit, watching over the pipeline stages, ensuring that nothing goes wrong. Implements [[ControlUnitIO]]
 */
class ControlUnit extends Module {
  val io = IO(new ControlUnitIO)
}

class ControlUnitIO extends Bundle {
  val in = Input(new ControlUnitInput)
  val out = Output(new ControlUnitOutput)

  class ControlUnitInput extends Bundle {

  }

  class ControlUnitOutput extends Bundle {

  }
}

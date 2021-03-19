package arithmetic

import chisel3._

import utils.Fixed._

/**
 * A fixed point adder/subtractor which works on [[utils.Fixed.FIXED_WIDTH]]-bit values. Implements [[ASUIO]]
 */
class FixedPointAddSub extends Module {
  val io = IO(new ASUIO)
  io.out.res := Mux(io.in.op, io.in.a-io.in.b, io.in.a+io.in.b)
  io.out.valid := io.in.valid
}

class ASUIO extends Bundle {
  val in = Input(new ASUInput)
  val out = Output(new ASUOutput)

  class ASUInput extends Bundle {
    val a = SInt(FIXED_WIDTH.W)
    val b = SInt(FIXED_WIDTH.W)
    val op = Bool()
    val valid = Bool()
  }

  class ASUOutput extends Bundle {
    val res = SInt(FIXED_WIDTH.W)
    val valid = Bool()
  }
}

object FixedPointAddSub {
  val SUB = true.B
  val ADD = false.B
}
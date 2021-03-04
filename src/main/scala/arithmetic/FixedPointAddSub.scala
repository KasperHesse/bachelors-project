package arithmetic

import chisel3._

import utils.Fixed._

/**
 * A fixed point adder/subtractor which works on [[utils.Fixed.FIXED_WIDTH]]-bit values<br>
 *
 *   <b> Ports </b> <br>
 * a: Input. SInt, width FIXED_WIDTH. First operand <br>
 * b: Input. SInt, width FIXED_WIDTH. Second operand <br>
 * op: Input. Bool. If op=0, res=a+b, else res=a-b <br>
 * res: Output. SInt, width FIXED_WIDTH
 */
class FixedPointAddSub extends Module {
  val io = IO(new ASUIO)

  io.out.res := Mux(io.in.op, io.in.a-io.in.b, io.in.a+io.in.b)
  io.out.done := io.in.en
}

class ASUIO extends Bundle {
  val in = Input(new ASUInput)
  val out = Output(new ASUOutput)
}

class ASUInput extends Bundle {
  val a = SInt(FIXED_WIDTH.W)
  val b = SInt(FIXED_WIDTH.W)
  val op = Bool()
  val en = Bool()
}

class ASUOutput extends Bundle {
  val res = SInt(FIXED_WIDTH.W)
  val done = Bool()
}

object FixedPointAddSub {
  val SUB = true.B
  val ADD = false.B
}
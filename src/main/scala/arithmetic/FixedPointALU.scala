package arithmetic

import chisel3._
import execution.Opcode
import utils.Fixed._

/**
 * A simple fixed point ALU which works on [[utils.Fixed.FIXED_WIDTH]]-bit values. Implements [[ASUIO]].
 * This ALU only supports opcodes ADD, SUB, MAX, MIN, ABS. For multiplications and divisions, use the designated multiplier and divisor.
 * If any opcode other than those supported is input, the output is the result of computing io.in.a + io.in.b
 */
class FixedPointALU extends Module {
  val io = IO(new ASUIO)
  io.out.res := io.in.a + io.in.b //Default assignment
  val max = Mux(io.in.a > io.in.b, io.in.a, io.in.b)
  val min = Mux(io.in.a < io.in.b, io.in.a, io.in.b)
  val abs = io.in.a.abs()
  when(io.in.op === Opcode.SUB) {
    io.out.res := io.in.a - io.in.b
  } .elsewhen(io.in.op === Opcode.MAX) {
    io.out.res := max
  } .elsewhen(io.in.op === Opcode.MIN) {
    io.out.res := min
  } .elsewhen(io.in.op === Opcode.ABS) {
    io.out.res := abs
  }
  io.out.valid := io.in.valid
}

/**
 * I/O bundle for the [[FixedPointALU]]
 */
class ASUIO extends Bundle {
  val in = Input(new ASUInput)
  val out = Output(new ASUOutput)

  class ASUInput extends Bundle {
    val a = SInt(FIXED_WIDTH.W)
    val b = SInt(FIXED_WIDTH.W)
    val op = Opcode()
    val valid = Bool()
  }

  class ASUOutput extends Bundle {
    val res = SInt(FIXED_WIDTH.W)
    val valid = Bool()
  }
}
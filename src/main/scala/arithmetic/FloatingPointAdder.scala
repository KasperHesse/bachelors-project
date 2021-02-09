package arithmetic

import chisel3._
import chisel3.util._
import utils.Float

/**
 * @brief Module which performs floating point addition or subtraction of two inputs.
 *
 * a (Float): First operand
 * b (Float): Second operand
 * op (U(2.W): Operation. Legal values are 0(add) and 1(sub)
 * res (Float): Result of calculation. If op=0, res=a+b. If op=1, res=a-b
 */
class FloatingPointAdder extends Module {
  val io = IO(new Bundle {
    val a = Input(new Float)
    val b = Input(new Float)
    val op = Input(UInt(2.W))
    val res = Output(new Float)
  })
}

/**
 * First stage of the floating-point adder. This stage compares exponents and shifts the significand of the lesser value to match the greater
 *
 * expa (U(11W): Exponent of the first value
 * expb (U(11W): Exponent of the second value
 * siga_in (U(56): Significand of the first value (inc guard,round,sticky bits)
 * sigb_in (U(56): Significand of the second value (inc guard,round,sticky bits)
 * siga_out (U(56W)): Updated significand of a
 * sigb_out (U(56W)): Updated significand of b
 * exp (U(11W)): Exponent of output value
 */
class FPAStage1 extends Module {
  val io = IO(new Bundle {
    val expa = Input(UInt(11.W))
    val expb = Input(UInt(11.W))
    val siga_in = Input(UInt(56.W))
    val sigb_in = Input(UInt(56.W))
//    val diff = Output(UInt(11.W))
    val siga_out = Output(UInt(55.W))
    val sigb_out = Output(UInt(55.W))
    val exp = Output(UInt(11.W))
  })

  val expa = RegNext(io.expa)
  val expb = RegNext(io.expb)
  val siga = RegNext(io.siga_in)
  val sigb = RegNext(io.sigb_in)

  val diff: SInt = (expa - expb).asSInt()

  val shiftIn = Wire(UInt(55.W))
  val shiftOut = Wire(UInt(55.W))

  when(diff > 0.S) { //A is larger, shift and increment b
    shiftIn := sigb
    io.siga_out := siga
    io.sigb_out := shiftOut
    io.exp := expa
  } .otherwise { //B is larger, shift and increment a
    shiftIn := siga
    io.siga_out := shiftOut
    io.sigb_out := sigb
    io.exp := expb
  }

  shiftOut := shiftIn >> Mux(diff > 0.S, diff.asUInt(), expb - expa)

//  io.diff := diff.asUInt()
}

/**
 * Second stage of the floating-point adder. Performs the add/sub of the significands
 */
class FPAStage2 extends Module {
  val io = IO(new Bundle {
    val siga = Input(UInt(56.W))
    val sigb = Input(UInt(56.W))
    val signa = Input(Bool())
    val signb = Input(Bool())
    val exp = Input(UInt(11.W))
    val op = Input(Bool())
    val sigOut = Output(UInt(56.W))
    val signOut = Output(Bool())
    val expOut = Output(UInt(11.W))
  })

  def twosComplement(v: UInt): UInt = {
    (~v).asUInt() + 1.U
  }

  //We need an additional bit on the significands to accept the carry out
  val siga = RegNext(Cat(0.U, io.siga))
  val sigb = RegNext(Cat(0.U, io.sigb))
  val signa = RegNext(io.signa)
  val signb = RegNext(io.signb)
  val exp = RegNext(io.exp)
  val op = RegNext(io.op)

  //Choose between 2's complement versions or original versions
  val a = Mux(signa, twosComplement(siga), siga)
  val b = Mux(signb ^ op, twosComplement(sigb), sigb)
  val intermediary = a + b
  //Choose whether significand result should be inverted again
  val twoCagain = (signa & intermediary(56)) | (signa & (signb ^ op)) | (intermediary(56) & (signb ^ op));

  io.sigOut := Mux(twoCagain, twosComplement(intermediary)(55,0), intermediary(55,0))
  io.signOut := twoCagain;
  io.expOut := exp
}

object FPcmd {
  val ADD = 0.U
  val SUB = 1.U
}



object FPAStage1 extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FPAStage1())
}
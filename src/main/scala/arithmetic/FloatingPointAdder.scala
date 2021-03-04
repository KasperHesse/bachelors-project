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
  val siga = Cat(1.U, Cat(io.a.man, 0.U(3.W)))
  val sigb = Cat(1.U, Cat(io.b.man, 0.U(3.W)))

  val stage1 = Module(new FPAStage1)
  val stage2 = Module(new FPAStage2)
  val stage3 = Module(new FPAStage3)
  val stage4 = Module(new FPAStage4)

  stage1.io.sigAIn := siga
  stage1.io.sigBIn := sigb
  stage1.io.expa := io.a.exp
  stage1.io.expb := io.b.exp
  stage1.io.signAIn := io.a.sign
  stage1.io.signBIn := io.b.sign
  stage1.io.opIn := io.op

  stage2.io.exp := stage1.io.exp
  stage2.io.siga := stage1.io.sigAOut
  stage2.io.sigb := stage1.io.sigBOut
  stage2.io.signa := stage1.io.signaOut
  stage2.io.signb := stage1.io.signbOut
  stage2.io.opIn := stage1.io.opOut

  stage3.io.expIn := stage2.io.expOut
  stage3.io.sigIn := stage2.io.sigOut
  stage3.io.signIn := stage2.io.signOut

  stage4.io.signIn := stage3.io.signOut
  stage4.io.expIn := stage3.io.expOut
  stage4.io.sigIn := stage3.io.sigOut

  io.res.sign := stage4.io.signOut
  io.res.exp := stage4.io.expOut
  io.res.man := stage4.io.sigOut(54,3)

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
    val sigAIn = Input(UInt(56.W))
    val sigBIn = Input(UInt(56.W))
    val signAIn = Input(Bool())
    val signBIn = Input(Bool())
    val opIn = Input(Bool())
    val sigAOut = Output(UInt(55.W))
    val sigBOut = Output(UInt(55.W))
    val exp = Output(UInt(11.W))
    val signaOut = Output(Bool())
    val signbOut = Output(Bool())
    val opOut = Output(Bool())
  })

  val expa = RegNext(io.expa)
  val expb = RegNext(io.expb)
  val siga = RegNext(io.sigAIn)
  val sigb = RegNext(io.sigBIn)
  val signa = RegNext(io.signAIn)
  val signb = RegNext(io.signBIn)
  val op = RegNext(io.opIn)

  val diff: SInt = (expa - expb).asSInt()

  val shiftIn = Wire(UInt(55.W))
  val shiftOut = Wire(UInt(55.W))

  when(diff > 0.S) { //A is larger, shift and increment b
    shiftIn := sigb
    io.sigAOut := siga
    io.sigBOut := shiftOut
    io.exp := expa
  } .otherwise { //B is larger, shift and increment a
    shiftIn := siga
    io.sigAOut := shiftOut
    io.sigBOut := sigb
    io.exp := expb
  }

  shiftOut := shiftIn >> Mux(diff > 0.S, diff.asUInt(), expb - expa)
  io.opOut := op
  io.signaOut := signa
  io.signbOut := signb

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
    val opIn = Input(Bool())
    val sigOut = Output(UInt(56.W))
    val signOut = Output(Bool())
    val expOut = Output(UInt(11.W))
  })

  def twosComplement(v: UInt): UInt = {
    (~v).asUInt() + 1.U
  }

  //We need an additional bit on the significands to accept the carry out
  val siga = RegNext(Cat(0.U, io.siga)) //57 bits
  val sigb = RegNext(Cat(0.U, io.sigb))
  val signa = RegNext(io.signa)
  val signb = RegNext(io.signb)
  val exp = RegNext(io.exp)
  val op = RegNext(io.opIn)

  //Choose between 2's complement versions or original versions
  val a = Mux(signa, twosComplement(siga), siga)
  val b = Mux(signb ^ op, twosComplement(sigb), sigb)
  val intermediary = a + b
  //Choose whether significand result should be inverted again
  val twoCagain = (signa & intermediary(56)) | (signa & (signb ^ op)) | (intermediary(56) & (signb ^ op));

  io.sigOut := Mux(twoCagain, twosComplement(intermediary), intermediary)
  io.signOut := twoCagain;
  io.expOut := exp
}

/**
 * Third stage of the adder, performing the normalization step.
 *
 * sigIn (U(57W)) [carry bit ; significand(53) ; GRS bits]
 * expIn (U(11W)) The current exponent estimate
 * signIn (Bool) The sign of the output
 * sigOut (U(56W)) [significand(53) ; GRS bits]. No need to pass the carry bit between S3 and S4
 * expOut (U(11W)) Updated exponent estimate, if necessary
 * signOut (Bool) Sign of the output
 * TODO: Add infinity checking
 */
class FPAStage3 extends Module {
  val io = IO(new Bundle {
    val sigIn = Input(UInt(57.W))
    val expIn = Input(UInt(11.W))
    val signIn = Input(Bool())
    val sigOut = Output(UInt(57.W))
    val expOut = Output(UInt(11.W))
    val signOut = Output(Bool())
  })

  val sig = RegNext(io.sigIn)
  val exp = RegNext(io.expIn)
  val sign = RegNext(io.signIn)

  val lzc = Module(new LeadingZerosCounter64Bit)
  lzc.io.in := sig << 7

  //if MSB of significand is 1, we normalize by shifting right by 1
  //if MSB is 0 but (MSB-1) is 1, we don't do anytning
  //Otherwise, we shift left by lzc and update accordingly
  when(sig(56)) {
    io.sigOut := sig >> 1
    io.expOut := exp + 1.U
  } .elsewhen (!sig(56) & sig(55)) {
    io.sigOut := sig
    io.expOut := exp
  } .otherwise {
    io.sigOut := sig << lzc.io.cnt
    io.expOut := exp - lzc.io.cnt
  }

  io.signOut := sign
}

/**
 * TODO: Add infinity checking at the end
 */
class FPAStage4 extends Module {
  val io = IO(new Bundle {
    val sigIn = Input(UInt(57.W))
    val expIn = Input(UInt(11.W))
    val signIn = Input(Bool())
    val sigOut = Output(UInt(57.W))
    val expOut = Output(UInt(11.W))
    val signOut = Output(Bool())
  })

  val sig = RegNext(io.sigIn)
  val exp = RegNext(io.expIn)
  val sign = RegNext(io.signIn)

  val sigRounded = Wire(UInt(57.W))

  //If GRS > 1/2 ULP, we round up
  //IF GRS > 1/2 ULP, we round down
  //IF GRS = 1/2 ULP, round to even
  when(sig(2) & (sig(1) | sig(0))) { //Round up
    sigRounded := sig + 4.U(57.W)
  } .elsewhen(!sig(2)) { //Round down
    sigRounded := sig
  } .otherwise { //Round to even
    sigRounded := sig + Cat(sig(3), 0.U(3.W)) //Add value at LSB of mantissa
  }

  //Check for rounding overflow. Can only occur when entire significand is 1
  when(sig(56, 3).andR()) {
    io.expOut := exp + 1.U
  }

  io.signOut := sign
  io.sigOut := sigRounded
  io.expOut := exp
}

object FPcmd {
  val ADD = 0.U
  val SUB = 1.U
}

object FPAStage1 extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FPAStage1())
}

object FloatingPointAdder extends App {
  (new chisel3.stage.ChiselStage).emitVerilog(new FloatingPointAdder())
}
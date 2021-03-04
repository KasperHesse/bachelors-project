package arithmetic

import chisel3._
import chisel3.util._
import utils.Fixed._
import utils.DivTypes._
import utils.MulTypes
import utils.Config.NRDIV_STAGE3_REPS

/**
 * Abstract base class for the fixed-point divider. Implements [[DivIO]]
 */
abstract class FixedPointDiv extends Module {
  val io = IO(new DivIO)
}

/**
 * I/O ports for a fixed-point divider
 */
class DivIO extends Bundle {
  val in = Input(new DivInput)
  val out = Output(new DivOutput)
}

/**
 * Input signals to a fixed-point divider
 */
class DivInput extends Bundle {
  /** Numerator of the fraction */
  val numer = SInt(FIXED_WIDTH.W)
  /** Denominator of the fraction */
  val denom = SInt(FIXED_WIDTH.W)
  /** Enable signal. Should be asserted when numer,denom are valid */
  val en = Bool()
}

/**
 * Output signals from a fixed-point divider
 */
class DivOutput extends Bundle {
  /** The result of computing numer/denom */
  val res = SInt(FIXED_WIDTH.W)
  /** Asserted for one clock cycle when the result is ready */
  val done = Bool()
}

/**
 * A fixed-point divider implemented using the Newton-Raphson method
 *
 * @note Should not be instantiated directly. Instead, use the companion object to generate the correct
 *       type of multiplier
 *
 */
class NRDiv extends FixedPointDiv {
  val stage1 = Module(new NRDivStage1)
  val stage2 = Module(new NRDivStage2)
  val stage3 = for (i <- 0 until NRDIV_STAGE3_REPS) yield {
    val stage = Module(new NRDivStage3)
    stage
  }
  val stage4 = Module(new NRDivStage4)

  stage1.io.in := io.in
  stage2.io.in <> stage1.io.out

  stage3(0).io.in <> stage2.io.out
  for(i <- 1 until NRDIV_STAGE3_REPS) {
    stage3(i).io.in <> stage3(i-1).io.out
  }

  stage4.io.in.numer := stage3(NRDIV_STAGE3_REPS-1).io.out.numer
  stage4.io.in.X := stage3(NRDIV_STAGE3_REPS-1).io.out.X
  stage4.io.in.neg := stage3(NRDIV_STAGE3_REPS-1).io.out.neg
  stage4.io.in.en := stage3(NRDIV_STAGE3_REPS-1).io.out.en

  io.out := stage4.io.out
}

class Stage1Output extends Bundle {
  val denom = SInt(FIXED_WIDTH.W)
  val numer = SInt(FIXED_WIDTH.W)
  val neg = Bool()
  val en = Bool()
}

class Stage1IO extends Bundle {
  val in = Input(new DivInput)
  val out = Output(new Stage1Output)
}

/**
 * First stage of the Newton-Raphson division circuit. Scales the denominator to be in range [0.5;1] and scales the
 * numerator by the same amount. The output denominator is the absolute value of the input denominator, and out.neg
 * is set true if in.denom was negative
 */
class NRDivStage1 extends Module {
  val io = IO(new Stage1IO)

  //Latch in signals, set up modules and constants
  val in = RegNext(io.in)
  val DIFF_WIDTH = log2Ceil(FIXED_WIDTH)+1 //Since it's fixed-point, we need an additional bit to ensure proper handling

  val neg = in.denom(FIXED_WIDTH-1)
  val lzc = Module(new LeadingZerosCounter64Bit)
  //Depending on test configuration, we must left-shift this by some fixed amount
  if(FIXED_WIDTH != 64) {
    lzc.io.in := Mux(neg, ((~in.denom).asSInt() + 1.S).asUInt(), in.denom.asUInt()) << (64 - FIXED_WIDTH)
  } else {
    lzc.io.in := Mux(neg, ((~in.denom).asSInt() + 1.S).asUInt(), in.denom.asUInt())
  }

  val diff = Wire(SInt(DIFF_WIDTH.W))
  val diffAbs = Wire(UInt(DIFF_WIDTH.W))
  diff := ((INT_WIDTH+1).U - lzc.io.cnt).asSInt()
  diffAbs := diff.abs.asUInt

  val shiftDir = diff(DIFF_WIDTH-1) //If 1, shift left by diffAbs, else shift right by diff

  //Must preserve the sign of numerator by keeping MSB constant and shifting all other bits
  val numerLeft1: Bits = in.numer << diffAbs
  val numerLeft: SInt = Cat(in.numer(FIXED_WIDTH - 1), numerLeft1(FIXED_WIDTH-2,0)).asSInt
  io.out.numer := Mux(shiftDir, numerLeft, in.numer >> diff.asUInt)

  //In the next steps, denom must be positive. We'll invert it here and re-invert in stage 4 if necessary
  val denomTemp = Mux(shiftDir, in.denom << diffAbs, in.denom >> diff.asUInt)
  io.out.denom := Mux(neg, (~denomTemp).asSInt() + 1.S, denomTemp)
  io.out.neg := neg
  io.out.en := in.en
}

class Stage2Output extends Bundle {
  val X = SInt(FIXED_WIDTH.W)
  val numer = SInt(FIXED_WIDTH.W)
  val denom = SInt(FIXED_WIDTH.W)
  val neg = Bool()
  val en = Bool()
}

class Stage2IO extends Bundle {
  val in = Input(new Stage1Output)
  val out = Output(new Stage2Output)
}
/**
 * Second stage of the Newton-Raphson division circuit.
 * Calculates the initial estimate X=48/17-32/17*dp, where dp is the scaled denominator
 */
class NRDivStage2 extends Module {
  val io = IO(new Stage2IO)

  val in = RegNext(io.in)

  val fortyEightOverSeventeen = double2fixed(48.0/17.0).S(FIXED_WIDTH.W)
  val thirtyTwoOverSeventeen = double2fixed(32.0/17.0).S(FIXED_WIDTH.W)

  val mul = Module(FixedPointMul(MulTypes.SINGLECYCLE))
  val sub = Module(new FixedPointAddSub)

  mul.io.a := in.denom
  mul.io.b := thirtyTwoOverSeventeen
  mul.io.en := in.en

  sub.io.in.a := fortyEightOverSeventeen
  sub.io.in.b := mul.io.res
  sub.io.in.op := FixedPointAddSub.SUB
  sub.io.in.en := mul.io.done

  io.out.X := sub.io.out.res
  io.out.numer := in.numer
  io.out.denom := in.denom
  io.out.neg := in.neg
  io.out.en := sub.io.out.done
}

class Stage3IO extends Bundle {
  val in = Input(new Stage2Output)
  val out = Output(new Stage2Output)
}

/**
 * Third stage of the Newton-Raphson division circuit.
 * Calculates one iteration of X = X + X*(1-D'*X) based on the current value of X.
 * The value of D' * X is stored in a register and fed into another multiplier to compute X*(1-D'*X)
 * This value is stored again, and in the final stage, X+(X*(1-D'*X)) is calculated
 * The total execution time for this circuit is 3 clock cycles
 */
class NRDivStage3 extends Module {
  val io = IO(new Stage3IO)
  val in = RegNext(io.in)

  val mul1 = Module(FixedPointMul(MulTypes.SINGLECYCLE))
  val mul2 = Module(FixedPointMul(MulTypes.SINGLECYCLE))

  val asu1 = Module(new FixedPointAddSub)
  val asu2 = Module(new FixedPointAddSub)

  //Stage 3.1 - Calculate D'*X
  mul1.io.a := in.X
  mul1.io.b := in.denom
  mul1.io.en := in.en

  val step1 = Wire(new Stage3InternalIO)
  step1.X := in.X
  step1.numer := in.numer
  step1.denom := in.denom
  step1.neg := in.neg
  step1.res := mul1.io.res //D' * X
  step1.en := mul1.io.done
  val step1Reg = RegNext(step1)

  //Stage 3.2 - Calculate X*(1-D'*x)
  asu1.io.in.op := FixedPointAddSub.SUB
  asu1.io.in.a := double2fixed(1).S(FIXED_WIDTH.W)
  asu1.io.in.b := step1Reg.res
  asu1.io.in.en := step1Reg.en

  mul2.io.a := asu1.io.out.res
  mul2.io.b := step1Reg.X
  mul2.io.en := asu1.io.out.done

  val step2 = Wire(new Stage3InternalIO)
  step2.X := step1Reg.X
  step2.numer := step1Reg.numer
  step2.denom := step1Reg.denom
  step2.neg := step1Reg.neg
  step2.res := mul2.io.res //X*(1-D' * X)
  step2.en := mul2.io.done
  val step2Reg = RegNext(step2)

  //Stage 3.3 - Calculate 1+X*(1-D'*x)
  asu2.io.in.a := step2Reg.X
  asu2.io.in.b := step2Reg.res
  asu2.io.in.op := FixedPointAddSub.ADD
  asu2.io.in.en := step2Reg.en

  io.out.X := asu2.io.out.res
  io.out.denom := step2Reg.denom
  io.out.numer := step2Reg.numer
  io.out.neg := step2Reg.neg
  io.out.en := asu2.io.out.done
}

class Stage4IO extends Bundle {
  val in = Input(new Stage4Input)
  val out = Output(new DivOutput)

  class Stage4Input extends Bundle {
    val X = SInt(FIXED_WIDTH.W)
    val numer = SInt(FIXED_WIDTH.W)
    val neg = Bool()
    val en = Bool()
  }
}

/**
 * Fourth stage of the Newton-Raphson division circuit.
 * This stage uses the value X=1/D' to calculate N/D as N'*X. Outputs the result of the division
 */
class NRDivStage4 extends Module {
  val io = IO(new Stage4IO)
  val in = RegNext(io.in)

  val mul = Module(FixedPointMul(MulTypes.SINGLECYCLE))
  mul.io.a := in.numer
  mul.io.b := in.X
  mul.io.en := in.en
  val temp = mul.io.res

  //Invert the sign of the result if necessary
  io.out.res := Mux(in.neg, (~temp).asSInt() + 1.S, temp)
  io.out.done := mul.io.done
}

class Stage3InternalIO extends Bundle {
  val X = SInt(FIXED_WIDTH.W)
  val numer = SInt(FIXED_WIDTH.W)
  val denom = SInt(FIXED_WIDTH.W)
  val neg = Bool()
  val en = Bool()
  val res = SInt(FIXED_WIDTH.W)
}

object FixedPointDiv {
  def apply(v: DivisorType): FixedPointDiv = {
    v match {
      case NEWTONRAPHSON => new NRDiv
      case _ => throw new IllegalArgumentException("Divisor type must be one of the set types")
    }
  }

  def apply(v: DivisorType, NRstage3reps: Int): FixedPointDiv = {
    v match {
      case NEWTONRAPHSON => new NRDiv
      case _ => throw new IllegalArgumentException("Can only create Newton-Raphson divisors with this apply()")
    }
  }
}


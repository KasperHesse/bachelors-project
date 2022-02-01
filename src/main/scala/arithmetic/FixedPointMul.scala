package arithmetic
import chisel3._
import chisel3.util._

import utils.Fixed._
import utils.MulTypes._

/**
 * Abstract base class for the fixed-point multiplier.
 * To instantiate a new multiplier, use the companion object to generate the correct kind.
 * Implements [[MulIO]]
 */
abstract class FixedPointMul extends Module {
  val io = IO(new MulIO)
}

/**
 * I/O Ports for fixed-point multipliers
 */
class MulIO extends Bundle {
  val in = Input(new MulInput)
  val out = Output(new MulOutput)

  class MulInput extends Bundle {
    /** First operand */
    val a = SInt(FIXED_WIDTH.W)
    /** Second operand */
    val b = SInt(FIXED_WIDTH.W)
    /** Data valid signal. Should be asserted for one clock cycle when a,b are valid */
    val valid = Bool()
  }

  class MulOutput extends Bundle {
    /** Asserted for one clock cycle when multiplication is finished and the output can be sampled */
    val valid = Bool()
    /** Output: Overflow bit, set high when the multiplication overflowed */
    val q = Bool()
    /** Output: The result of a*b */
    val res = SInt(FIXED_WIDTH.W)
  }
}

/**
 * A single-cycle fixed point multiplier.
 *
 * @note Should not be instantiated directly. Instead, use the companion object to generate the correct
 *       type of multiplier
 *
 */
class FixedMulSingleCycle extends FixedPointMul {
  val prod = Wire(SInt((2*FIXED_WIDTH).W))
  prod := io.in.a*io.in.b
  // When multiplying Qa.b numbers, we get a Q(2a).(2b) number
  //We must right-shift by "b" to get a Q(2a).b number.
  val prod2 = (prod >> FRAC_WIDTH).asSInt()
 //To ensure proper rounding, we add the digit which was shifted off the end
  val lastBit = Cat(0.S(1.W), prod(FRAC_WIDTH - 1).asSInt()).asSInt()
  io.out.res := prod2 + lastBit

  //Set overflow bit properly. Overflow starts at bit FIXED_WIDTH+FRAC_WIDTH
  io.out.q := prod(2*FIXED_WIDTH-1,FIXED_WIDTH+FRAC_WIDTH) =/= 0.U
  io.out.valid := io.in.valid
}

class FixedMulMultiCycle extends FixedPointMul {
  val prod = Wire(SInt((2*FIXED_WIDTH).W))
  prod := io.in.a*io.in.b
  val prod2 = (prod >> FRAC_WIDTH).asSInt()
  val lastBit = Cat(0.S(1.W), prod(FRAC_WIDTH-1).asSInt()).asSInt()

  val tmp = RegNext(prod2)
  val validReg = RegNext(io.in.valid)
  val lastBitReg = RegNext(lastBit)

  io.out.res := tmp + lastBitReg
  io.out.q := false.B //TODO implement overflow checking. Not relevant as of yet
  io.out.valid := validReg
}

class FixedMulKaratsuba extends FixedPointMul {

  //Perform multiplication on the abs-values, flip sign afterwards
  val a_abs = io.in.a.abs
  val b_abs = io.in.b.abs

  val a_sign = io.in.a(FIXED_WIDTH-1)
  val b_sign = io.in.b(FIXED_WIDTH-1)

  val a_high = Wire(UInt((FIXED_WIDTH/2).W))
  val a_low = Wire(UInt((FIXED_WIDTH/2).W))
  val b_high = Wire(UInt((FIXED_WIDTH/2).W))
  val b_low = Wire(UInt((FIXED_WIDTH/2).W))

  val z1 = Wire(UInt((FIXED_WIDTH+2).W))

  a_high := a_abs(FIXED_WIDTH-1, FIXED_WIDTH/2)
  a_low  := a_abs(FIXED_WIDTH/2-1, 0)
  b_high := b_abs(FIXED_WIDTH-1, FIXED_WIDTH/2)
  b_low := b_abs(FIXED_WIDTH/2-1, 0)

  val z2 = a_high * b_high
  val z0 = a_low * b_low

  val ahl = a_low  +& a_high
  val bhl = b_high +& b_low
  val z1_1 = (ahl * bhl)
  val z1_2 = z1_1 - z2
  z1 := (z1_2 - z0)

  val prod = (z2 << (FIXED_WIDTH)).asUInt + (z1 << FIXED_WIDTH/2).asUInt + z0.asUInt

  val prod2 = (prod >> FRAC_WIDTH).asUInt
  val lastBit = Cat(0.S(1.W), prod(FRAC_WIDTH-1)).asUInt
  val res = prod2 + lastBit

  io.out.res := Mux(a_sign ^ b_sign, (~res).asSInt + 1.S, res.asSInt)

//  io.out.res := prod2 + lastBit
  io.out.q := prod(2*FIXED_WIDTH-1,FIXED_WIDTH+FRAC_WIDTH) =/= 0.U
  io.out.valid := io.in.valid
}

/**
 * Companion object for the fixed-point multipliers. Should be used to instantiate new multipliers
 */
object FixedPointMul {
  /**
   * Generates a new fixed-point multiplier of the type defined in the parameter
   * @param v The type of multiplier to instantiate. See [[utils.MulTypes]]
   * @return A new multiplier of the given type
   */
  def apply(v: MulType): FixedPointMul = {
    v match {
      case SINGLECYCLE => new FixedMulSingleCycle
      case MULTICYCLE => new FixedMulMultiCycle
      case KARATSUBA => new FixedMulKaratsuba

      case _ => throw new IllegalArgumentException("Only single-cycle multipliers are supported as of right now")
    }
  }
}

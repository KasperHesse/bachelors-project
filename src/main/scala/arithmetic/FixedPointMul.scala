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
      case _ => throw new IllegalArgumentException("Only single-cycle multipliers are supported as of right now")
    }
  }
}

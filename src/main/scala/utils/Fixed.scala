package utils

import chisel3._

/**
 * This object contains all constants and methods relating to operating with fixed-point numbers.
 */
object Fixed {
  //Default config: Fracwidth 48, intwidth 15, fixedwidth 64
  /** Number of bits in the fractional part of a fixed-point number */
  val FRAC_WIDTH = 38
  /** Number of bits in the integer part of a fixed-point number */
  val INT_WIDTH = 15
  /** Number of bits total that a fixed-point number takes up */
  val FIXED_WIDTH = 54

  /**
   * Converts a given double value into its corresponding fixed-point representation
   * @param value The number to convert
   * @return A long corresponding to the bit pattern of that number
   */
  def double2fixed(value: Double): Long = {
    var v = math.round(value*math.pow(2,FRAC_WIDTH))
    if(v >= math.pow(2,FIXED_WIDTH-1).toLong) {
      v = math.pow(2,FIXED_WIDTH-1).toLong - 1L
    }
    v
  }

  /**
   * Converts a given fixed-point number (stored as an SInt) into the double which it represents
   * @param value The number to convert
   * @return A double with the value that the fixed-point number represents
   */
  def sint2double(value: SInt): Double = {
    value.litValue().toDouble * math.pow(2,-FRAC_WIDTH)
  }

  /**
   * Converts a given fixed-point number (as a long) into the double which it represents
   * @param value The number to convert
   * @return A double with the value that the fixed-point number represents
   */
  def fixed2double(value: Long): Double = {
    value.toDouble * math.pow(2,-FRAC_WIDTH)
  }

  /**
   * Adds two floating-point numbers the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a+b, within in the limits of the addition hardware
   */
  def fixedAdd(x: Double, y: Double): Double = {
    fixed2double(fixedAdd(double2fixed(x), double2fixed((y))))
  }
  /**
   * Adds two longs representing fixed-point numbers
   * the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a+b, within in the limits of the addition hardware
   */
  def fixedAdd(x: Long, y: Long): Long = {
    var e = x+y
    if(e < -math.pow(2,FIXED_WIDTH-1)) {
       e + math.pow(2,FIXED_WIDTH).toLong
    } else if (e > math.pow(2, FIXED_WIDTH-1)-1) {
      (e - math.pow(2, FIXED_WIDTH)).toLong
    } else {
      e
    }

  }

  /**
   * Subtracts two floating-point numbers the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a-b, within in the limits of the addition hardware
   */
  def fixedSub(x: Double, y: Double): Double = {
    fixed2double(fixedSub(double2fixed(x), double2fixed((y))))
  }

  /**
   * Subtracts two longs representing fixed-point numbers,
   * the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a-b, within the limits of the addition hardware
   */
  def fixedSub(x: Long, y: Long): Long = {
    val e = x-y
    if(e < -math.pow(2,FIXED_WIDTH-1)) {
      (e + math.pow(2,FIXED_WIDTH)).toLong
    } else if (e > math.pow(2, FIXED_WIDTH-1)-1) {
      (e - math.pow(2, FIXED_WIDTH)).toLong
    } else {
      e
    }
  }
  /**
   * Multiply two doubles the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a*b, within in the limits of the addition hardware
   *
   */
  def fixedMul(x: Double, y: Double): Double = {
    fixed2double(fixedMul(double2fixed(x), double2fixed(y)))
  }

  /**
   * Multiply two longs representing fixed-point numbers
   * the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a*b, within in the limits of the addition hardware
   *
   */
  def fixedMul(x: Long, y: Long): Long = {
    val a = BigInt(x)
    val b = BigInt(y)
    //The below is ugly but necessary. See module implementation for explanation
    val e = (((a*b) >> FRAC_WIDTH) + ((a*b) >> (FRAC_WIDTH-1) & 1)) & (BigInt(2).pow(FIXED_WIDTH)-1)

    if(e > (BigInt(2).pow(FIXED_WIDTH-1)-1)) {
      (e-BigInt(2).pow(FIXED_WIDTH)).toLong
    } else if (e < -BigInt(2).pow(FIXED_WIDTH-1)) {
      (e+BigInt(2).pow(FIXED_WIDTH)).toLong
    } else {
      e.toLong
    }
  }



  /**
   * Generates a double within in the range expressible by the fixed-point numbers used.
   * Highest possible value is 2^INT_WIDTH^-2^-FRAC_WIDTH^.
   * Lowest possible value is -1*2^INT_WIDTH^
   * @return A double
   */
  def getDouble(): Double = {
    val r = scala.util.Random
    val gen = r.nextDouble() * math.pow(2, INT_WIDTH) * (if (r.nextBoolean()) 1 else -1)
    if(gen == math.pow(2,INT_WIDTH)) {
      gen - math.pow(2,-FRAC_WIDTH)
    } else gen
  }

  if(FRAC_WIDTH+INT_WIDTH != FIXED_WIDTH-1) {
    throw new IllegalArgumentException("Width of Fixed-point numbers must be 1(sign) + frac_width+int_width")
  }
}
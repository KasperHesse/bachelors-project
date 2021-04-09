package utils

import chisel3._

/**
 * This object contains all constants and methods relating to operating with fixed-point numbers.
 */
object Fixed {
  //Default config: Fracwidth 48, intwidth 15, fixedwidth 64
  /** Number of bits in the fractional part of a fixed-point number */
  var FRAC_WIDTH = 38
  /** Number of bits in the integer part of a fixed-point number */
  var INT_WIDTH = 15
  /** Number of bits total that a fixed-point number takes up */
  var FIXED_WIDTH = 54

  require(FRAC_WIDTH+INT_WIDTH == FIXED_WIDTH-1, "Width of Fixed-point numbers must be 1(sign) + frac_width+int_width")

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
  def fixed2double(value: SInt): Double = {
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
   * Convert an SInt to the long literal that its interger part represents
   * @param value The SInt to convert
   * @return A long representing the same integer part
   */
  def fixed2long(value: SInt): Long = {
    value.litValue.toLong >> FRAC_WIDTH
  }

  /**
   * Converts a long literal to an SInt with the same value
   * @param value The long to convert
   * @return An SInt representing the same value
   */
  def long2fixed(value: Long): SInt = {
    (value << FRAC_WIDTH).S(FIXED_WIDTH.W)
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

  def fixedAdd(x: SInt, y: SInt): SInt = {
    fixedAdd(x.litValue().toLong, y.litValue.toLong).S
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
   * Subtracts two SInts representing fixed-point numbers,
   * the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a-b, within the limits of the addition hardware
   */
  def fixedSub(x: SInt, y: SInt): SInt = {
    fixedSub(x.litValue().toLong, y.litValue.toLong).S(FIXED_WIDTH.W)
  }

  /**
   * Multiply two doubles the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a*b, within in the limits of the hardware
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
   * @return a*b, within in the limits of the hardware
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
   * Multiply two SInts representing fixed-point numbers
   * the way the hardware would (within range [-1*2^INT_WIDTH^ ;2^INT_WIDTH^-1])
   * @param x The first number
   * @param y The second number
   * @return a*b, within in the limits of the hardware
   */
  def fixedMul(x: SInt, y: SInt): SInt = {
    fixedMul(x.litValue().toLong, y.litValue.toLong).S
  }

  /**
   * Divides two SInts the way the hardware would (to some degree of precision, at least)
   * @param n The numerator
   * @param d The denominator
   * @return the fixed-point number representing n/d
   */
  def fixedDiv(n: SInt, d: SInt): SInt = {
    double2fixed(fixed2double(n) / fixed2double(d)).S(FIXED_WIDTH.W)
//    (fixed2long(n)/fixed2long(d)).S(FIXED_WIDTH.W)
  }

  /**
   * Computes the fraction n/d by using the newton-raphson division method
   * @param numer Numerator
   * @param denom Denominator
   * @return The quotient n/d, expressed as a long
   */
  def nrDiv(numer: Long, denom: Long): Long = {
    //Stage 1: Count leading zeros
    //Scale denominator between 0.5 and 1, scale numerator by same amount
    var cntZeros = 0
    var dd = if(denom < 0) ~denom + 1 else denom //Properly handle two's complement
    var nn = numer
    if(FIXED_WIDTH < 64) {
      dd = dd << (64-FIXED_WIDTH)
    }
//    while((dd & ((BigInt(1) << FIXED_WIDTH-1)).toLong) == 0) {
    while((dd & 0x8000000000000000L) == 0) {
      dd = dd << 1
      cntZeros = cntZeros + 1
    }
    //Difference from 16 spaces
    val diff = INT_WIDTH - cntZeros
    if(diff > 0) { //We must right-shift
      dd = denom >> diff
      nn = numer >> diff
    } else if (diff < 0) { //We must left-shift
      //Important: If left-shifting, we must preserve the sign bit of the original value. Only left-shift the 63 bits that make up the number?
      //Alternatively, work with BigInts from here on?
      dd = denom << -1*diff
      nn = ((numer << -1*diff) & 0x7fffffffffffffffL) | (numer & 0x8000000000000000L)
    } else {
      dd = denom
      nn = numer
    }
    //Problem at this stage: If left-shifting, we may severely mess up our value of the numerator
    print(s"Original d: ${fixed2double(denom)}, scaled d: ${fixed2double(dd)}\n")
    print(s"Original n: ${fixed2double(numer)}, scaled n: ${fixed2double(nn)}\n")
    //Firstly. This only works if d is positive. If negative, invert it and then re-invert the result

    //Stage 2: Calculate the value 48/17-32/17*D'
    if(denom < 0) dd = ~dd + 1
    var X = fixedSub(double2fixed(48.0/17.0),fixedMul(double2fixed(32.0/17.0),dd))
    for(i <- 0 until 4) {
      X = fixedAdd(X,fixedMul(X, fixedSub(double2fixed(1), fixedMul(dd,X))))
    }
    val reslong = fixedMul(nn, X) * (if(denom < 0) -1 else 1)
    val res = fixed2double(reslong)
    print(s"NR-div with fixeds gives $numer/$denom=$res\n")
//    print(s"Deviation: ${n/d-res}\n")
    reslong
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
}
package utils

import chisel3._
import execution.RtypeInstruction

/**
 * This object contains all constants and methods relating to operating with fixed-point numbers.
 */
object Fixed {
  //Default config: Fracwidth 38, intwidth 15, fixedwidth 54
  /** Number of bits in the fractional part of a fixed-point number */
  var FRAC_WIDTH = 38
  /** Number of bits in the integer part of a fixed-point number */
  var INT_WIDTH = 15
  /** Number of bits total that a fixed-point number takes up */
  var FIXED_WIDTH = 54

  /** Number of bits in the integer part of an immediate value (excluding sign bit) */
  val IMM_INT_WIDTH = 3
  /** Number of bits in the fractional part of an immediate value */
  val IMM_FRAC_WIDTH = 7

  require(FRAC_WIDTH+INT_WIDTH == FIXED_WIDTH-1, "Width of Fixed-point numbers must be 1(sign) + frac_width+int_width")

  /**
   * Converts a given double value into its corresponding fixed-point representation
   * Values outside of the possible range are saturated at the largest possible value
   * @param value The number to convert
   * @return A long corresponding to the bit pattern of that number
   */
  def double2fixed(value: Double): Long = {
    var v = math.round(value*math.pow(2,FRAC_WIDTH))
    if(v >= math.pow(2,FIXED_WIDTH-1).toLong) {
      v = math.pow(2,FIXED_WIDTH-1).toLong - 1L
    } else if (v < (-math.pow(2, FIXED_WIDTH-1).toLong)) {
      v = (-math.pow(2, FIXED_WIDTH-1).toLong)
    }
    v
  }

  /**
   * Parses a string containing a hexadecimal value, returning the fixed-point value that this string represents
   * @param value A string containing a hexadecimal value
   * @return
   */
  def string2fixed(value: String): SInt = {
    val x = java.lang.Long.parseUnsignedLong(value, 16) & ((1L << FIXED_WIDTH) -1L)
    val y = if (x >= Math.pow(2, FIXED_WIDTH-1).toLong) x - Math.pow(2, FIXED_WIDTH).toLong else x
    y.S(FIXED_WIDTH.W)
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
    fixed2double(fixedAdd(double2fixed(x), double2fixed(y)))
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
    fixedAdd(x.litValue().toLong, y.litValue.toLong).S(FIXED_WIDTH.W)
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
    fixedMul(x.litValue().toLong, y.litValue.toLong).S(FIXED_WIDTH.W)
  }

  /**
   * Divides two SInts the way the hardware would (to some degree of precision, at least)
   * @param n The numerator
   * @param d The denominator
   * @return the fixed-point number representing n/d. If d==0, returns 0 instead (as the hardware does)
   */
  def fixedDiv(n: SInt, d: SInt): SInt = {
    if(d.litValue == 0) return 0.S(FIXED_WIDTH.W)
    double2fixed(fixed2double(n) / fixed2double(d)).S(FIXED_WIDTH.W)
  }

  /**
   * Calculates a square root in the same manner that the hardware would
   * @param v The value to calculate the square root of
   * @return The square root of v
   */
  def fixedSqrt(v: Double): Double = {
    fixed2double(fixedSqrt(double2fixed(v).S(FIXED_WIDTH.W)))
  }
  /**
   * Calculates a square root in the same manner that the hardware would using the Babylonian method
   * @param S The value to calculate the square root of
   * @return The square root of v
   */
  def fixedSqrt(S: SInt): SInt = {
    var x0 = fixedMul(S, double2fixed(0.5).S(FIXED_WIDTH.W)) //Initial estimate, S/2
    val onehalf = double2fixed(0.5).S(FIXED_WIDTH.W) //Constant: 1/2
    var xnew = 0.S(FIXED_WIDTH.W) //New value
    for(i <- 0 until 6) {
      xnew = fixedMul(onehalf, fixedAdd(x0, fixedDiv(S, x0)))
      x0 = xnew
    }
    xnew
  }

  /**
   * Calculates a square root in the same manner that the hardware would.
   * @param S The value to calculate the square root of. Should be a fixed-point value
   * @return The square root of v
   */
  def fixedSqrt(S: Long): Long = {
    fixedSqrt(S.S(FIXED_WIDTH.W)).litValue.toLong
  }

  /**
   * Returns the largest of two fixed-point numbers
   * @param a The first value
   * @param b The second value
   * @return The largest of the two inputs
   */
  def fixedMax(a: Long, b: Long): Long = {
    if (a > b) a else b
  }

  /**
   * Returns the largest of two fixed-point numbers
   * @param a The first value
   * @param b The second value
   * @return The largest of the two inputs
   */
  def fixedMax(a: SInt, b: SInt): SInt = {
    fixedMax(a.litValue.toLong, b.litValue.toLong).S(FIXED_WIDTH.W)
  }

  /**
   * Returns the largest of two doubles, when interpreted as fixed-point numbers
   * @param a The first value
   * @param b The second value
   * @return The largest of the two when interpreted as fixed-point numbers
   */
  def fixedMax(a: Double, b: Double): Double = {
    fixed2double(fixedMax(double2fixed(a), double2fixed(b)))
  }

  /**
   * Returns the smallest of two fixed-point numbers
   * @param a The first value
   * @param b The second value
   * @return The smallest of the two inputs
   */
  def fixedMin(a: Long, b: Long): Long = {
    if (a < b) a else b
  }

  def fixedMin(a: SInt, b: SInt): SInt = {
    fixedMin(a.litValue().toLong, b.litValue().toLong).S(FIXED_WIDTH.W)
  }

  /**
   * Returns the smallest of two doubles, when interpreted as fixed-point numbers
   * @param a The first value
   * @param b The second value
   * @return The smallest of the two when interpreted as fixed-point numbers
   */
  def fixedMin(a: Double, b: Double): Double = {
    fixed2double(fixedMin(double2fixed(a), double2fixed(b)))
  }

  /**
   * Returns the absolute value of the input
   * @param a The input value
   * @return The absolute value of that value
   */
  def fixedAbs(a: Double): Double = {
    math.abs(a)
  }

  /**
   * Returns the absolute value of the input
   * @param a The input value
   * @return The absolute value of that value
   */
  def fixedAbs(a: Long): Long = {
    double2fixed(math.abs(fixed2double(a)))
  }

  /**
   * Returns the absolute value of the input
   * @param a The input value
   * @return The absolute value of that value
   */
  def fixedAbs(a: SInt): SInt = {
    fixedAbs(a.litValue.toLong).S(FIXED_WIDTH.W)
  }

  /**
   * Converts an immediate value to its Qs3.7 representation as a fixed-point number.
   * Bits 0:6 are the fractional part, and bits 7:10 are the integer part, with bit 10 being the sign bit
   * @param imm
   * @return
   */
  def imm2fixed(imm: Double): Long = {
    val immRounded = math.round(imm*math.pow(2,IMM_FRAC_WIDTH))*math.pow(2,-IMM_FRAC_WIDTH)
    require(immRounded >= -math.pow(2,IMM_INT_WIDTH) && immRounded < math.pow(2, IMM_INT_WIDTH), s"Immediate value must be between ${-math.pow(2,IMM_INT_WIDTH)} and ${math.pow(2,IMM_INT_WIDTH)-math.pow(2,-IMM_FRAC_WIDTH)}")
    val immfixed = math.round(imm*math.pow(2,IMM_FRAC_WIDTH))
    immfixed
  }

  /**
   * Converts a fixed-point immediate into its constituent parts
   * @param imm The fixed-point immediate value in Qs3.7 format
   * @return An array holding two bit patterns. At (0) the integer part of the immediate and at (1) the fractional part
   */
  def fixedImm2parts(imm: Long): Array[Int] = {
    val immfrac = imm & ((1 << IMM_FRAC_WIDTH) -1)
    val immh = (imm >> IMM_FRAC_WIDTH) & ((1 << IMM_INT_WIDTH+1)-1)
    Array(immh.toInt, immfrac.toInt)
  }

  /**
   * Computes the immediate value stored in an Rtype instruction
   * @param instr The instruction
   * @return The immediate value encoded in the instruction
   */
  def getImmediate(instr: RtypeInstruction): SInt = {
    val immh = instr.rs1.litValue.toInt
    val immfrac = instr.immfrac.litValue.toInt
    val neg = (immh & 0x8) > 0
    //If negative, we utilize the fact that 16-immh gives the correct negative version of our value
    val high = if(neg) -1*(16-immh) else immh
    val imm: Double = high + immfrac*math.pow(2,-7)

    return double2fixed(imm).S(FIXED_WIDTH.W)
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
  def genDouble(): Double = {
    val r = scala.util.Random
    var gen = r.nextDouble() * math.pow(2, INT_WIDTH) * (if (r.nextBoolean()) 1 else -1)
    if(gen == math.pow(2,INT_WIDTH)) {
      gen = gen - math.pow(2, -FRAC_WIDTH)
    }
    gen
  }

  /**
   * Generates a double which can fit inside an R-type instructions immediate field.
   * Highest possible value is 2^[[IMM_INT_WIDTH^]]^-2^-[[IMM_FRAC_WIDTH]]^.
   * Lowest possible value is -1*2^[[IMM_INT_WIDTH]]^
   * @return A double which will fit inside the immediate field
   */
  def genImmediate(): Double = {
    val r = scala.util.Random
    var gen = r.nextDouble() * math.pow(2,IMM_INT_WIDTH) * (if (r.nextBoolean()) 1 else -1)
    if(gen == math.pow(2,IMM_INT_WIDTH)) {
      gen = gen - math.pow(2,-IMM_FRAC_WIDTH)
    }
    gen
  }
}
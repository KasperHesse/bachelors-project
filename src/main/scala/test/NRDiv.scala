package test

import utils.Fixed._

object NRDiv extends App {
  NRDivWithFixeds()

  def NRDivWithDoubles(): Unit = {
    val r = scala.util.Random;
    val denom = r.nextDouble()*math.pow(2,5)*(if(r.nextBoolean())1 else -1)
    val numer = r.nextDouble()*math.pow(2,5)*(if(r.nextBoolean())1 else -1)

    //Extract exponent field of denominator to realise amount of bitshift
    val exp1 = (java.lang.Double.doubleToRawLongBits(denom) & 0x7ff0000000000000L) >>> 52

    val sign1 = denom < 0
    val sign2 = numer < 0

    //Run the thing. Note that it doesn't work that well with negative operands, so we need to invert them if negative
    val dp = denom / math.pow(2, ((exp1 - 1023) + 1).toDouble) * (if (sign1) -1 else 1)
    val np = numer / math.pow(2, ((exp1 - 1023) + 1).toDouble) * (if (sign2) -1 else 1)
    var X = 48 / 17 - 32 / 17 * dp; //Magic constants
    for (i <- 0 until 4) {
      X = X + X * (1 - dp * X)
    }
    val res = np * X * (if (sign1 ^ sign2) -1 else 1) //Fix the sign based on the operands
    print(s"$numer/$denom=${numer/denom}, NR-method=$res\n")
  }

  def NRDivWithFixeds(): Unit = {
    val r = scala.util.Random
    val d = r.nextDouble()*math.pow(2,5)*(if(r.nextBoolean())1 else -1)
    val n = r.nextDouble()*math.pow(2,5)*(if(r.nextBoolean())1 else -1)
    //Throw over to longs to work with fixed-points
    val denom = double2fixed(d)
    val numer = double2fixed(n)

    //Stage 1: Count leading zeros
    //Scale denominator between 0.5 and 1, scale numerator by same amount
    var cntZeros = 0
    var dd = if(denom < 0) ~denom + 1 else denom //Properly handle two's complement
    var nn = numer
    while((dd & 0x8000000000000000L) == 0) {
      dd = dd << 1
      cntZeros = cntZeros + 1
    }
    //Difference from 16 spaces
    val diff = 16 - cntZeros
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
    printf(s"Original d: ${fixed2double(denom)}, scaled d: ${fixed2double(dd)}\n")
    printf(s"Original n: ${fixed2double(numer)}, scaled n: ${fixed2double(nn)}\n")
    //Firstly. This only works if d is positive. If negative, invert it and then re-invert the result

    //Stage 2: Calculate the value 48/17-32/17*D'
    if(d < 0) dd = ~dd + 1
    var X = fixedSub(double2fixed(48.0/17.0),fixedMul(double2fixed(32.0/17.0),dd))
    for(i <- 0 until 4) {
      X = fixedAdd(X,fixedMul(X, fixedSub(double2fixed(1), fixedMul(dd,X))))
    }
    val res = fixed2double(fixedMul(nn, X)) * (if(d < 0) -1 else 1)
    print(s"$n/$d=${n/d}. NR-div with fixeds gives $res\n")
    print(s"Deviation: ${n/d-res}\n")

  }


}

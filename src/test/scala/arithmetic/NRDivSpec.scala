package arithmetic

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._

class NRDivSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Newton-Raphson division module"

  /**
   * Generates two random denominator and numerator values, and outputs these + the scaled versions
   * @return A sequence containing
   *         [0]=unscaled denominator (Long)
   *         [1]=unscaled numerator (Long)
   *         [2]=scale denominator, absolute value (Long)
   *         [3]=scaled numerator (Long)
   *         [4]=sign of numerator, -1 or 1 (Int)
   *         [5]=original denominator (Double)
   *         [6]=original numerator (Double)
   */
  def generateDenomNumer(): Seq[AnyVal] = {
    val x = getDouble()
    val y = getDouble()
    val denom = double2fixed(x)
    val numer = double2fixed(y)
//    val denom = -57L
//    val numer = 77L
    //Count number of leading zeros
    var cntZeros = 0
    val neg = denom < 0
    var dp = if(neg) ~denom + 1 else denom //Properly handle two's complement
    var np = numer
    while((dp & (1L << FIXED_WIDTH-1)) == 0) {
      dp = dp << 1
      cntZeros = cntZeros + 1
    }

    //Shift accordingly
    val diff = (INT_WIDTH+1) - cntZeros
    if(diff > 0) { //Right-shift
      dp = denom >> diff
      np = numer >> diff
    } else if (diff < 0) { //Left-shift
      //Important: If left-shifting, we must preserve the sign of the original value
      dp = denom << -1*diff
      val npUpper = (-1 << FIXED_WIDTH-1) & numer //Keep upper bits, including sign
      val npLower = (numer & ((1 << (FIXED_WIDTH+diff-1)) -1 )) << -1*diff //Left-shift the appropriate portion of numer
      np = npUpper | npLower
    } else {
      dp = denom
      np = numer
    }
    if(denom < 0) dp = (~dp) + 1L

    Seq(denom, numer, dp, np, if(neg) -1 else 1, x, y)
  }

  def genX(dp: Long): Long = {
    fixedSub(double2fixed(48.0/17.0),fixedMul(double2fixed(32.0/17.0),dp))
  }

  def iterateX(dp: Long, X: Long): Long = {

    val mul1 = fixedMul(dp, X)
    val sub1 = fixedSub(double2fixed(1), mul1)
    val mul2 = fixedMul(X, sub1)
    val add2 = fixedAdd(X, mul2)
    add2
  }

  //Test whether shifts are correctly happening in stage 1
  def testStage1(dut: NRDivStage1, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val denom = gen(0).asInstanceOf[Long]
      val numer = gen(1).asInstanceOf[Long]
      val dp = gen(2).asInstanceOf[Long]
      val np = gen(3).asInstanceOf[Long]
      val neg = if (gen(4).asInstanceOf[Int] < 0) true else false

      dut.io.in.numer.poke(numer.S)
      dut.io.in.denom.poke(denom.S)
      dut.clock.step()
      if((dut.io.out.numer.peek.litValue().toLong != np) || (dut.io.out.denom.peek.litValue().toLong != dp)) {
        print(s"denom=$denom (${fixed2double(denom)}), numer=$numer (${fixed2double(numer)})\n")
        print(s"dp=$dp (${fixed2double(dp)}), out.denom=${dut.io.out.denom.peek()}\n")
        print(s"np=$np (${fixed2double(np)}), out.numer=${dut.io.out.numer.peek()}\n")
        print(s"Sign: $neg\n")
      }
      dut.io.out.numer.expect(np.S)
      dut.io.out.denom.expect(dp.S)
      dut.io.out.neg.expect(neg.B)
    }
  }

  def testStage2(dut: NRDivStage2, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val dp = gen(2).asInstanceOf[Long]
      val np = gen(3).asInstanceOf[Long]
      val neg = if(gen(4).asInstanceOf[Int] < 0) true else false

      dut.io.in.denom.poke(dp.S)
      dut.io.in.numer.poke(np.S)
      dut.io.in.neg.poke(neg.B)
      dut.clock.step()

      dut.io.out.denom.expect(dp.S)
      dut.io.out.numer.expect(np.S)
      dut.io.out.neg.expect(neg.B)

      val X = genX(dp)
      dut.io.out.X.expect(X.S)
    }
  }

  def testStage3(dut: NRDivStage3, iters: Int): Unit = {
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val dp = gen(2).asInstanceOf[Long]
      val np = gen(3).asInstanceOf[Long]
      val neg = if (gen(4).asInstanceOf[Int] < 0) true else false
      val X0 = genX(dp)
      val X1 = iterateX(dp, X0)

      dut.io.in.X.poke(X0.S)
      dut.io.in.numer.poke(np.S)
      dut.io.in.denom.poke(dp.S)
      dut.io.in.neg.poke(neg.B)
      dut.clock.step(4)

      dut.io.out.X.expect(X1.S)
      dut.io.out.numer.expect(np.S)
      dut.io.out.denom.expect(dp.S)
      dut.io.out.neg.expect(neg.B)
    }
  }

  def testStage4(dut: NRDivStage4, iters: Int): Unit = {
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val dp = gen(2).asInstanceOf[Long]
      val np = gen(3).asInstanceOf[Long]
      val neg = if (gen(4).asInstanceOf[Int] < 0) true else false
      val X0 = genX(dp)
      val X1 = iterateX(dp, X0)
      val X2 = iterateX(dp, X1)
      val X3 = iterateX(dp, X2)
      val X4 = iterateX(dp, X3)
      val e = fixedMul(np, X4)
      val res = if(neg) (~e) + 1L else e

      dut.io.in.X.poke(X4.S)
      dut.io.in.numer.poke(np.S)
      dut.io.in.neg.poke(neg.B)

      dut.clock.step()
      dut.io.out.res.expect(res.S)
    }
  }


  def NRDivBenchmark(dut: NRDiv, iters: Int): Unit = {

    val denoms = new Array[Long](iters)
    val numers = new Array[Long](iters)
    val results = new Array[Double](iters)
    val xs = new Array[Double](iters)
    val ys = new Array[Double](iters)

    var maxdiff = Double.MinValue
    var mindiff = Double.MaxValue
    var sumdiff = 0.0
    //Generate stimuli
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val denom = gen(0).asInstanceOf[Long]
      val numer = gen(1).asInstanceOf[Long]
      val x = gen(5).asInstanceOf[Double]
      val y = gen(6).asInstanceOf[Double]

      denoms(i) = denom
      numers(i) = numer
      results(i) = y/x
      xs(i) = x
      ys(i) = y
    }

    var i = 0
    var resultCnt = 0
    while(resultCnt < iters && i < 500) {
      if(i < iters) {
        dut.io.in.numer.poke(numers(i).S)
        dut.io.in.denom.poke(denoms(i).S)
        dut.io.in.valid.poke(true.B)
      } else if (i >= iters/2+2 && i < iters+2) {
        dut.io.in.numer.poke(numers(i-2).S)
        dut.io.in.denom.poke(denoms(i-2).S)
        dut.io.in.valid.poke(true.B)
      } else {
        dut.io.in.valid.poke(false.B)
      }
      dut.clock.step()
      if(dut.io.out.valid.peek().litToBoolean) {
        val orig = ys(resultCnt)/xs(resultCnt)
        val got = fixed2double(dut.io.out.res.peek.litValue.toLong)
        val diff = math.abs(orig-got)

        maxdiff = math.max(maxdiff,diff)
        mindiff = math.min(mindiff,diff)
        sumdiff += diff
        resultCnt += 1
      }
      i += 1
    }
    print(s"NR divider with ${dut.stage3Reps} repetitions in stage 3. $iters iterations. Q$INT_WIDTH.$FRAC_WIDTH\n")
    print(s"Largest deviation from real result: $maxdiff\n")
    print(s"Smallest devitation from real result: $mindiff\n")
    print(s"Average deviation from real result: ${sumdiff/iters}\n\n")
  }

  def NRDivMultiTest(dut: NRDiv, iters: Int): Unit = {
    val denoms = new Array[Long](iters)
    val numers = new Array[Long](iters)
    val results = new Array[Double](iters)

    //Generate stimuli
    for(i <- 0 until iters) {
      val gen = generateDenomNumer()
      val denom = gen(0).asInstanceOf[Long]
      val numer = gen(1).asInstanceOf[Long]
      val x = gen(5).asInstanceOf[Double]
      val y = gen(6).asInstanceOf[Double]

      denoms(i) = denom
      numers(i) = numer
      results(i) = y/x
    }

    var i = 0
    var resultCnt = 0
    while(resultCnt < iters && i < 500) {
      if(i < iters/2) {
        dut.io.in.numer.poke(numers(i).S)
        dut.io.in.denom.poke(denoms(i).S)
        dut.io.in.valid.poke(true.B)
      } else if (i >= iters/2+2 && i < iters+2) {
        dut.io.in.numer.poke(numers(i-2).S)
        dut.io.in.denom.poke(denoms(i-2).S)
        dut.io.in.valid.poke(true.B)
      } else {
        dut.io.in.valid.poke(false.B)
      }
      dut.clock.step()
      if(dut.io.out.valid.peek().litToBoolean) {
        assert(math.abs(results(resultCnt) - fixed2double(dut.io.out.res.peek)) < 1E-2)
        resultCnt += 1
      } else if(i > 10) {
      }
      i += 1
    }
  }

  val iters = 200
 /*
  it should "shift denominator and numerator" in {
    test(new NRDivStage1()).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      testStage1(c, iters)
    }
  }

  it should "calculate the initial estimate 48/17-32/17*dp" in {
    test(new NRDivStage2()) {c =>
      testStage2(c, iters)
    }
  }

  it should "iterate to a new value of X" in {
    test(new NRDivStage3()).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      testStage3(c, iters)
    }
  }

  it should "multiply N and X to generate the result of the division" in {
    test(new NRDivStage4()) { c =>
      testStage4(c, iters)
    }
  }

  it should "be able to handle a stream of divisions, with a break in the middle" in {
    test(new NRDiv()).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      NRDivMultiTest(c, iters)
    }
  }

  it should "benchmark the precision" in {
    for(i <- 1 to 4) {
      test(new NRDiv(i)) {c =>
        NRDivBenchmark(c, iters)
      }
    }
  } */

  it should "perform 1/3" in {
        FIXED_WIDTH = 16
        INT_WIDTH = 7
        FRAC_WIDTH = 8
        NRDIV_STAGE3_REPS = 4

    test(new NRDiv()) {dut =>
      val op1 = double2fixed(9)
      val op2 = double2fixed(25)
      dut.io.in.numer.poke(op1.S)
      dut.io.in.denom.poke(op2.S)
      print(s"op1=${op1}(${fixed2double(op1)}, op2=${op2}(${fixed2double(op2)})\n")
      dut.io.in.valid.poke(true.B)
      while(!dut.io.out.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      print(s"Got: ${dut.io.out.res.peek} / ${fixed2double(dut.io.out.res.peek)}\n")
      nrDiv(op1, op2)
    }
  }
}

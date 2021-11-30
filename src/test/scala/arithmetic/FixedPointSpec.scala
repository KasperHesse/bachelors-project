package arithmetic

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import execution.Opcode
import utils.Fixed._
import execution.Opcode._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{WriteVcdAnnotation, VerilatorBackendAnnotation}


class FixedPointSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Fixed Point Circuitry"

  def testAddition(dut: FixedPointALU, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 to iters) {
      //Generate random doubles which fit in INT_WIDTH bits
      val x = genDouble()
      val y = genDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.io.in.op.poke(ADD)
      dut.clock.step()
      val res = fixedAdd(a,b)

      //Ugly hack that's necessary due to bit-level errors once FIXED_WIDTH goes above 54
      assert(math.abs(dut.io.out.res.peek.litValue().toLong - res) < (1 << FIXED_WIDTH-53))
    }
  }

  def testSubtraction(dut: FixedPointALU, iters: Int): Unit = {
    val r = scala.util.Random
    for (i <- 0 to iters) {
      //Generate random doubles which fit in INT_WIDTH bits
      val x = genDouble()
      val y = genDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.io.in.op.poke(SUB)
      dut.clock.step()
      val res = fixedSub(a,b)
      //Ugly hack that's necessary due to bit-level errors once FIXED_WIDTH goes above 54
      assert(math.abs(dut.io.out.res.peek.litValue().toLong - res) < (1 << FIXED_WIDTH-53))
    }
  }

  def testMaxMin(dut: FixedPointALU, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val x = genDouble()
      val y = genDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      val bool = r.nextBoolean()
      val op = if (bool) Opcode.MAX else Opcode.MIN
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.io.in.op.poke(op)
      dut.clock.step()
      val res = if (bool) fixedMax(a,b) else fixedMin(a,b)
      dut.io.out.res.expect(res.S)
    }
  }

  def testMultiplication(dut: FixedPointMul, iters: Int):Unit = {
    for(i <- 0 until iters) {
      val x = genDouble()
      val y = genDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
//      val a = 0x33e
//      val b = 0x227
//      val a = -94
//      val b = -431
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.clock.step()
      val res = fixedMul(a,b)
      println(f"a=$a/$x%.3f, b=$b/$y%.3f, res=$res/${fixed2double(res)}%.3f. Peeking ${dut.io.out.res.peek()}")
      dut.io.out.res.expect(res.S)
    }
  }

  def multiplyNegativeOne(dut: FixedPointMul, iters: Int):Unit = {

    val a0 = double2fixed(0)
    val b0 = string2fixed("3ffe5e527029ab")

    val a = double2fixed(0)
    val b = string2fixed("00020a159dc7ef")

    val c = double2fixed(-1)
    val d = string2fixed("3fffedf1d00c38")


    dut.io.in.a.poke(a0.S)
    dut.io.in.b.poke(b0)
    dut.io.in.valid.poke(true.B)
    dut.clock.step()


    dut.io.in.a.poke(a.S)
    dut.io.in.b.poke(b)
    dut.clock.step()

    dut.io.in.a.poke(c.S)
    dut.io.in.b.poke(d)
    dut.clock.step()
    val res = fixedMul(c.S,d)
    dut.io.out.res.expect(res)

  }

  def testMultiplicationOverflow(dut: FixedPointMul, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val x = genDouble()
      val y = genDouble()
      val a = BigInt(double2fixed(x))
      val b = BigInt(double2fixed(y))
      val e = (a*b) >> (FRAC_WIDTH+FIXED_WIDTH)
      val o = e != 0
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.clock.step()
      dut.io.out.q.expect(o.B)
    }
  }

  it should "correctly add numbers" in {
    test(new FixedPointALU) { c =>
      testAddition(c, 100)
    }
  }

  it should "correctly subtract numbers" in {
    test(new FixedPointALU) { c =>
      testSubtraction(c, 20)
    }
  }

  it should "correctly multiply numbers in a single cycle" in {
    test(FixedPointMul(utils.MulTypes.KARATSUBA)) { c =>
      testMultiplication(c, 20)
    }
  }

  it should "correct multiply numbers over multiple clock cycles" in {
    test(FixedPointMul(utils.MulTypes.MULTICYCLE)) {dut =>
      testMultiplication(dut, 20)
    }
  }

  it should "correctly multiply numbers using karatsuba" in {
//    FIXED_WIDTH = 10
//    INT_WIDTH = 4
//    FRAC_WIDTH = 5
    test(FixedPointMul(utils.MulTypes.KARATSUBA)).withAnnotations(Seq(WriteVcdAnnotation, VerilatorBackendAnnotation)) {dut =>
      testMultiplication(dut, 50)
    }
  }

  it should "multiply with negative one" in {
    test(FixedPointMul(utils.MulTypes.SINGLECYCLE)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      multiplyNegativeOne(dut, 5)
    }
  }

  "FixedPointAlu" should "correctly assert max or min" in {
    test(new FixedPointALU) {dut =>
      testMaxMin(dut, 20)
    }
  }

  it should "multiply with zero" in {
    test(FixedPointMul(utils.Config.MULTYPE)) { dut =>
      dut.io.in.a.poke(long2fixed(5))
      dut.io.in.b.poke(long2fixed(0))
      dut.io.in.valid.poke(true.B)
      dut.clock.step()

      while(!dut.io.out.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      dut.io.out.res.expect(0.S)

    }
  }

  it should "correctly assert overflow when multiplying" in {
    test(FixedPointMul(utils.Config.MULTYPE)) { c =>
      testMultiplicationOverflow(c, 5)
    }
  }


}

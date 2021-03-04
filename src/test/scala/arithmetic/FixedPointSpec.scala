package arithmetic

import java.math.BigInteger

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._

class FixedPointSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Fixed Point Circuitry"

  def testAddition(dut: FixedPointAddSub, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 to iters) {
      //Generate random doubles which fit in INT_WIDTH bits
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      val op = false
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.io.in.op.poke(op.B)
      dut.clock.step()
      val res = fixedAdd(a,b)

      //Ugly hack that's necessary due to bit-level errors once FIXED_WIDTH goes above 54
      assert(math.abs(dut.io.out.res.peek.litValue().toLong - res) < (1 << FIXED_WIDTH-53))
    }
  }

  def testSubtraction(dut: FixedPointAddSub, iters: Int): Unit = {
    val r = scala.util.Random
    for (i <- 0 to iters) {
      //Generate random doubles which fit in INT_WIDTH bits
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      val op = true
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.io.in.op.poke(op.B)
      dut.clock.step()
      val res = fixedSub(a,b)
      //Ugly hack that's necessary due to bit-level errors once FIXED_WIDTH goes above 54
      assert(math.abs(dut.io.out.res.peek.litValue().toLong - res) < (1 << FIXED_WIDTH-53))
    }
  }

  def testMultiplication(dut: FixedPointMul, iters: Int):Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      dut.io.a.poke(a.S)
      dut.io.b.poke(b.S)
      dut.clock.step()
      val res = fixedMul(a,b)
      dut.io.res.expect(res.S)
    }
  }

  def testMultiplicationOverflow(dut: FixedPointMul, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val x = getDouble()
      val y = getDouble()
      val a = BigInt(double2fixed(x))
      val b = BigInt(double2fixed(y))
      val e = (a*b) >> (FRAC_WIDTH+FIXED_WIDTH)
      val o = e != 0
      dut.io.a.poke(a.S)
      dut.io.b.poke(b.S)
      dut.clock.step()
      dut.io.q.expect(o.B)
    }
  }

  def testStuff(dut: FixedPointMul): Unit = {
    dut.io.a.poke(0x7f.S)
    dut.io.b.poke(0x7f.S)
    dut.clock.step()
    dut.io.res.expect((-16).S)
  }

  it should "correctly add numbers" in {
    test(new FixedPointAddSub) { c =>
      testAddition(c, 100)
    }
  }

  it should "correctly subtract numbers" in {
    test(new FixedPointAddSub) { c =>
      testSubtraction(c, 20)
    }
  }

  it should "correctly multiply numbers in a single cycle" in {
    test(FixedPointMul(utils.Config.MULTYPE)) { c =>
      testMultiplication(c, 20)
    }
  }

  it should "correctly assert overflow when multiplying" in {
    test(FixedPointMul(utils.Config.MULTYPE)) { c =>
      testMultiplicationOverflow(c, 5)
    }
  }



}

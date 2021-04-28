package arithmetic

import java.math.BigInteger

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.Opcode._
import vector.Opcode

class FixedPointSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Fixed Point Circuitry"

  def testAddition(dut: FixedPointALU, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 to iters) {
      //Generate random doubles which fit in INT_WIDTH bits
      val x = getDouble()
      val y = getDouble()
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
      val x = getDouble()
      val y = getDouble()
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
      val x = getDouble()
      val y = getDouble()
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
    val r = scala.util.Random
    for(i <- 0 until iters) {
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      dut.io.in.a.poke(a.S)
      dut.io.in.b.poke(b.S)
      dut.clock.step()
      val res = fixedMul(a,b)
      dut.io.out.res.expect(res.S)
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
    test(FixedPointMul(utils.Config.MULTYPE)) { c =>
      testMultiplication(c, 20)
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

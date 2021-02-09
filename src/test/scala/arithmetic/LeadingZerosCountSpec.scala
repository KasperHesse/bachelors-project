package arithmetic

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{Matchers, FlatSpec}

class LeadingZerosCountSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "LeadingZerosCounter"

  def LZC32(dut: LeadingZerosCounter32Bit, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      var x = r.nextLong() >>> 32 //Squeeze down to int-size
      var cnt = 0
      dut.io.in.poke(x.asUInt())
      dut.clock.step()

      //Calculate answer
      while ((x >>> 31) == 0)  {
        x = x << 1
        cnt+=1
      }
//      print(s"Poking with ${x.toBinaryString} Expecting $cnt, got ${dut.io.cnt.peek()}\n")
      dut.io.cnt.expect(cnt.U)
    }
  }

  def LZC64(dut: LeadingZerosCounter64Bit, iters: Int): Unit = {
    val r = scala.util.Random
    for(i <- 0 until iters) {
      var x = r.nextLong() >>> 1 //Squeeze
      var cnt = 0
      dut.io.in.poke(x.asUInt())
      dut.clock.step()

      //Calculate answer
      while ((x >>> 63) == 0)  {
        x = x << 1
        cnt+=1
      }
//      print(s"Poking with ${x.toBinaryString} Expecting $cnt, got ${dut.io.cnt.peek()}\n")
      dut.io.cnt.expect(cnt.U)
    }
  }

  def testQ32(dut: LeadingZerosCounter32Bit): Unit = {
    dut.io.in.poke(0.U)
    dut.clock.step()
    dut.io.Q.expect(true.B)
  }

  def testQ64(dut: LeadingZerosCounter64Bit): Unit = {
    dut.io.in.poke(0.U)
    dut.clock.step()
    dut.io.Q.expect(true.B)
  }

  it should "correctly find the number of leading zeros in a 32-bit value" in {
    test(new LeadingZerosCounter32Bit) { c =>
      LZC32(c, 20)
    }
  }
  it should "correctly find the number of leading zeros in a 64-bit value" in {
    test(new LeadingZerosCounter64Bit) {c => LZC64(c, 20)}
  }

  it should "correctly assert Q in the 32-bit case" in {
    test(new LeadingZerosCounter32Bit) {c => testQ32(c)}
  }

  it should "correctly assert Q in the 64-bit case" in {
    test(new LeadingZerosCounter64Bit) {c => testQ64(c)}
  }
}

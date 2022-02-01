package test

import chiseltest._
import chisel3._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class InSystemMemoryContentSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers  {
  it should "generate vcd" in {
    test(new InSystemMemoryContent(8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(200)
    }
  }
}

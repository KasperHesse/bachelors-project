package test

import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import chisel3._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class InSystemMemoryContentSpec extends FlatSpec with ChiselScalatestTester with Matchers  {
  it should "generate vcd" in {
    test(new InSystemMemoryContent(8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(200)
    }
  }
}

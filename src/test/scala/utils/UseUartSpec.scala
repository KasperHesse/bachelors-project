package utils

import chisel3._
import chisel3.tester.experimental.TestOptionBuilder._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import org.scalatest.{FlatSpec, Matchers}

class UseUartSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "UseUart"

  it should "transmit some binary data" in {
    test(new UseUart).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(400)
    }
  }
}

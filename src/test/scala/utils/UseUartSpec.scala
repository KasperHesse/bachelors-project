package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UseUartSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "UseUart"

  it should "transmit some binary data" in {
    test(new UseUart).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(400)
    }
  }
}

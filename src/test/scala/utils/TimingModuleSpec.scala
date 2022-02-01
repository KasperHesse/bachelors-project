package utils

import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class TimingModuleSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Timing Module"

  it should "output a vcd file" in {
    test(new TimingModule(2000)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.en.poke(true.B)
      dut.clock.step(120)
      dut.io.en.poke(false.B)
      dut.clock.step(10)
      dut.io.en.poke(true.B)
      dut.clock.step(10)
      dut.io.clr.poke(true.B)
      dut.clock.step(5)
      dut.io.clr.poke(false.B)
      dut.clock.step(20)
    }
  }
}

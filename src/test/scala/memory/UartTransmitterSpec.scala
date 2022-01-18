package memory

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder.ChiselScalatestOptionBuilder
import chiseltest.internal.WriteVcdAnnotation
import execution.StypeBaseAddress
import org.scalatest.{FlatSpec, Matchers}
import utils.Config.NUM_MEMORY_BANKS
import utils.Fixed.FIXED_WIDTH

class UartTransmitterSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Uart Transmitter"

  it should "transmit some data and generate vcd" in {
    test(new UartTransmitter(20, 10)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //Setup data
      dut.clock.setTimeout(0)
      dut.io.vec.bits.baseAddr.poke(StypeBaseAddress.UART)
      dut.io.vec.valid.poke(false.B)

      dut.io.wrData.valid.poke(false.B)
      dut.io.wrData.bits.wrData(0).poke(3.S(FIXED_WIDTH.W))
      dut.io.wrData.bits.wrData(1).poke((-1).S(FIXED_WIDTH.W))
      for(i <- 2 until NUM_MEMORY_BANKS-2) {
        dut.io.wrData.bits.wrData(i).poke((10+i).S(FIXED_WIDTH.W))
      }
      dut.io.wrData.bits.wrData(6).poke((7L << FIXED_WIDTH-4).S(FIXED_WIDTH.W))
      dut.io.wrData.bits.wrData(7).poke((7L << FIXED_WIDTH-4).S(FIXED_WIDTH.W))
      dut.clock.step()

      //First cc: uart valid goes high
      //id and wrdata should both be ready
      dut.io.vec.valid.poke(true.B)
      dut.io.vec.ready.expect(true.B)
      dut.io.wrData.ready.expect(true.B)
      dut.clock.step()

      //Next cc: wrData.valid goes high
      //id.ready should be false, wrData.ready should still be true
      dut.io.wrData.valid.poke(true.B)
      dut.io.vec.ready.expect(false.B)
      dut.io.wrData.ready.expect(true.B)
      dut.clock.step()

      //Next cc: both ready signals should be low
      //wrData.valid will be low
      dut.io.vec.ready.expect(false.B)
      dut.io.wrData.ready.expect(false.B)
      dut.io.wrData.valid.poke(false.B)

      dut.io.vec.valid.poke(true.B)
      dut.clock.step(2000)

    }
  }

}

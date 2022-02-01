package test

import chisel3._
import chisel3.util._
import chiseltest._
import utils.Config._
import utils.Fixed._
import execution.StypeBaseAddress
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class UsingSyncReadMemSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Using sync-read mem"

  it should "write and read values" in {
    test(new UsingSyncReadMem).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //Setup handshake
      dut.io.en.poke(true.B)
      dut.io.re.poke(true.B)
      for(i <- 0 until 2) {
        dut.io.addr(i).poke(i.U)
        dut.io.wrData(i).poke((i + 2).U)
        dut.io.we(i).poke(false.B)
      }

      //Read values
      dut.clock.step()
      for(i <- 0 until 2) {
        dut.io.rdData(i).expect((i).U)
      }

      //Write new
      for(i <- 0 until 2) {
        dut.io.we(i).poke(true.B)
      }
      dut.io.re.poke(false.B)
      dut.io.en.poke(true.B)
      dut.clock.step()

      //Expect old values to be kept
      for(i <- 0 until 2) {
        dut.io.rdData(i).expect(i.U)
      }

      //Read new
      dut.io.re.poke(true.B)
      for(i <- 0 until 2) {
        dut.io.we(i).poke(false.B)
      }
      dut.clock.step()
      for(i <- 0 until 2) {
        dut.io.rdData(i).expect((i+2).U)
      }

      //Write values
//      dut.io.en.poke(true.B)
//      dut.io.re.poke(false.B)
//      for(i <- 0 until 2){
//        dut.io.addr(i).poke(4.U)
//        dut.io.wrData(i).poke(8.U)
//        dut.io.we(i).poke(true.B)
//      }
//      dut.clock.step()
//
//      for(i <- 0 until 2) {
//        dut.io.we(i).poke(false.B)
//      }
//
//      dut.io.re.poke(true.B)
//
//      dut.clock.step()
//      for(i <- 0 until 2) {
//        dut.io.rdData(i).expect(8.U)
//      }
    }
  }
}

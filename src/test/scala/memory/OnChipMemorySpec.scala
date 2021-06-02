package memory

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import pipeline.StypeBaseAddress

class OnChipMemorySpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "On-chip memory"

  //We'll attempt to read some values at subsequent locations 0-7, 8-15 etc. Each bank holds 8 words in this test
  it should "support read operations" in {
    simulationConfig()
    test(new OnChipMemory(8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>

      //Poke read addr and setup handshake signals
      dut.io.wb.ready.poke(true.B)
      dut.io.addrGen.valid.poke(true.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke((i+8).U)
        dut.io.addrGen.bits.validAddress(i).poke(true.B)
      }
      dut.clock.step()
      //Expect output values
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i+8).S)
      }
    }
  }

  it should "keep output values when ready is deasserted" in {
    simulationConfig()
    test(new OnChipMemory(8)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //Poke read addr and setup handshake signals
      dut.io.wb.ready.poke(true.B)
      dut.io.addrGen.valid.poke(true.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke((i+8).U)
        dut.io.addrGen.bits.validAddress(i).poke(true.B)
      }
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i+8).S)
      }
      dut.io.wb.valid.expect(true.B)

      //Verify that outputs are held once wb.ready goes false
      dut.io.wb.ready.poke(false.B)
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i+8).S)
      }
      dut.io.wb.valid.expect(false.B)

      //Try new values, just to make sure it wasn't a fluke
      dut.io.wb.ready.poke(true.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke((i).U)
        dut.io.addrGen.bits.validAddress(i).poke(true.B)
      }
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i).S)
      }
      dut.io.wb.valid.expect(true.B)
    }
  }

  it should "accept writes when we is asserted" in {
    simulationConfig()
    test(new OnChipMemory(8)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //Setup handshake and values
      dut.io.addrGen.valid.poke(true.B)
      dut.io.wb.ready.poke(true.B)
      dut.io.we.poke(false.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke(i.U)
        dut.io.addrGen.bits.validAddress(i).poke(true.B)
        dut.io.writeQueue.bits.wrData(i).poke(double2fixed(i+8).S)
      }

      //First, read values to verify their existence
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i).S)
      }

      //Write values
      dut.io.we.poke(true.B)
      dut.io.wb.ready.poke(false.B) //Notice that we can still write when wb is not ready
      dut.clock.step(1)
      dut.io.wb.valid.expect(false.B)

      //Read values out
      dut.io.we.poke(false.B)
      dut.io.wb.ready.poke(true.B)
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.wb.bits.rdData(i).expect(double2fixed(i+8).S)
      }

    }
  }

  it should "only write the indices that are valid" in {
    simulationConfig()
    test(new OnChipMemory(8)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //Setup handshake and values
      dut.io.addrGen.valid.poke(true.B)
      dut.io.we.poke(true.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke(i.U)
        dut.io.addrGen.bits.validAddress(i).poke((i < 4).B)
        dut.io.writeQueue.bits.wrData(i).poke(double2fixed(i+8).S)
      }
      //Step in values
      dut.clock.step()

      //Set up read access
      dut.io.we.poke(false.B)
      dut.io.addrGen.bits.validAddress.foreach(_.poke(true.B))
      dut.io.wb.ready.poke(true.B)
      dut.clock.step()

      //Verify outputs
      for(i <- 0 until NUM_MEMORY_BANKS) {
        val e = if(i < 4) (i+8) else i
        dut.io.wb.bits.rdData(i).expect(double2fixed(e).S)
      }
    }
  }

  it should "read 0 when indices are not valid" in {
    simulationConfig()
    test(new OnChipMemory(8)) {dut =>
      dut.io.addrGen.valid.poke(true.B)
      dut.io.wb.ready.poke(true.B)
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.addrGen.bits.addr(i).poke(i.U)
        dut.io.addrGen.bits.validAddress(i).poke((i < 4).B)
      }
      dut.clock.step()
      for(i <- 0 until NUM_MEMORY_BANKS) {
        val e = if(i < 4) i else 0
        dut.io.wb.bits.rdData(i).expect(double2fixed(e).S)
      }
    }
  }
}

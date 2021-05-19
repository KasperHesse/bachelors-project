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

class AddressGeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers  {

  "Address generator" should "reorder inputs based on the LSB" in {
    test(new AddressGenerator()) {dut =>
      //Generate input vectors
      val addr = Seq(1,2,3,4,5,6,7,0)
      val valid = Seq(true,false,true,false,true,false,true,false)

      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.in.bits.indices(i).poke(addr(i).U)
        dut.io.in.bits.validIndices(i).poke(valid(i).B)
      }
      dut.io.in.bits.baseAddr.poke(StypeBaseAddress.X)
      val baseAddr = AddressDecode.mapping(StypeBaseAddress.X.litValue().toInt)
      dut.clock.step()
      //Expect elem 0 at location 1
      dut.io.mem.bits.addr(0).expect(baseAddr.U)
      dut.io.mem.bits.validAddress(0).expect(false.B)
      for(i <- 1 until NUM_MEMORY_BANKS) {
        dut.io.mem.bits.addr(i).expect((addr(i-1)+baseAddr).U)
        dut.io.mem.bits.validAddress(i).expect(valid(i-1).B)
      }
    }
  }

  "Address generator" should "prioritize later inputs" in {
    test(new AddressGenerator()) {dut =>
      for(i <- 0 until NUM_MEMORY_BANKS) {
        dut.io.in.bits.indices(i).poke((i << 3).U)
      }
      dut.clock.step()
      dut.io.mem.bits.addr(0).expect(((NUM_MEMORY_BANKS-1) << 3).U)
      for(i <- 1 until NUM_MEMORY_BANKS) {
        dut.io.mem.bits.addr(i).expect((i << 3).U)
      }

      for(i <- 0 until NUM_MEMORY_BANKS-1) {
        dut.io.in.bits.indices(i).poke(0.U)
        dut.io.in.bits.validIndices(i).poke(false.B)
      }
      dut.io.in.bits.indices(NUM_MEMORY_BANKS-1).poke(0.U)
      dut.io.in.bits.validIndices(NUM_MEMORY_BANKS-1).poke(true.B)
      dut.clock.step()

      dut.io.mem.bits.validAddress(NUM_MEMORY_BANKS-1).expect(true.B)
    }
  }
}

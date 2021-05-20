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



  def pokeIndex(dut: AddressGenerator, index: Int, value: Int, valid: Boolean = true): Unit = {
    dut.io.in.bits.indices(index).poke(value.U)
    dut.io.in.bits.validIndices(index).poke(valid.B)
  }

  def expectAddr(dut: AddressGenerator, addr: Int, value: Int, valid: Boolean = true): Unit = {
    dut.io.mem.bits.addr(addr).expect(value.U)
    dut.io.mem.bits.validAddress(addr).expect(valid.B)
  }

  "Address generator" should "reorder inputs based on the LSB" in {
    test(new AddressGenerator()) {dut =>
      //Generate input vectors
      val addr = Seq(1,2,3,4,5,6,7,0)
      val valid = Seq(true,false,true,false,true,false,true,false)
      //Only input indices 0,2,4,6 are valid
      //Only output indices 1,3,5,7 should be valid
      //Output indices 0,2,4,6 should keep their input values but have valid=false
      for(i <- 0 until NUM_MEMORY_BANKS) {
        pokeIndex(dut, i, addr(i), valid(i))
      }
      dut.io.in.bits.baseAddr.poke(StypeBaseAddress.X)
      val baseAddr = AddressDecode.mapping(StypeBaseAddress.X.litValue().toInt)
      dut.clock.step()
      expectAddr(dut, 0, baseAddr + 1, false)
      expectAddr(dut, 1, baseAddr + 1)
      expectAddr(dut, 2, baseAddr + 3, false)
      expectAddr(dut, 3, baseAddr + 3)
      expectAddr(dut, 4, baseAddr + 5, false)
      expectAddr(dut, 5, baseAddr + 5)
      expectAddr(dut, 6, baseAddr + 7, false)
      expectAddr(dut, 7, baseAddr + 7)
    }
  }

  "Address generator" should "prioritize later inputs" in {
    test(new AddressGenerator()) {dut =>
      for(i <- 0 until NUM_MEMORY_BANKS) {
        pokeIndex(dut, i, i << 3) //0, 8, 16, 24 etc. All have LSB 000
      }
      dut.clock.step()
      //Expect the latest value to have be the only one that succeeded
      expectAddr(dut, 0, (NUM_MEMORY_BANKS-1) << 3)
      for(i <- 1 until NUM_MEMORY_BANKS) {
        dut.io.mem.bits.validAddress(i).expect(false.B)
      }
    }
  }

  "Address generator" should "only reorder inputs that have the valid flag set" in {
    test(new AddressGenerator()) {dut =>
      //Attempt to poke two elements that map to the same bank.
      //One is valid, the other is invalid
      pokeIndex(dut, 0, 2) //true
      pokeIndex(dut, 1, 10, valid = false)
      for(i <- 2 until NUM_MEMORY_BANKS) {
        dut.io.in.bits.validIndices(i).poke(false.B)
      }
      dut.io.in.valid.poke(true.B)
      dut.io.mem.ready.poke(true.B)
      dut.clock.step()
      for(i <- 0 until 2) {
        dut.io.mem.bits.validAddress(i).expect(false.B)
      }
      expectAddr(dut, 2, 2)
      for(i <- 3 until NUM_MEMORY_BANKS) {
        dut.io.mem.bits.validAddress(i).expect(false.B)
      }
    }
  }
}

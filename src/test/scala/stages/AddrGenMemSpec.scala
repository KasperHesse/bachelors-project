package memory.substages

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import execution.StypeBaseAddress._
import execution.StypeBaseAddress

class AddrGenMemSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  def pokeAddr(dut: AddrGenMem, index: Int, addrOffset: Int, valid: Boolean = true): Unit = {
    dut.io.in.bits.indices(index).poke(addrOffset.U)
    dut.io.in.bits.validIndices(index).poke(valid.B)
  }

  def expectValue(dut: AddrGenMem, index: Int, value: Int, valid: Boolean = true, baseAddress: StypeBaseAddress.Type = X): Unit = {
    val e = if(valid) (value + baseAddress.litValue.toInt).S else 0.S
    dut.io.out.bits.rdData(index).expect(e)
  }

  "Address generator and memory" should "read subsequent values" in {
    SIMULATION = true
    test(new AddrGenMem(8, "src/test/scala/memory/membankinit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.out.valid.expect(false.B)

      var readCnt = 0 //Number of times we've read data
      var writeCnt = 0 //Number of times we've written new addresses to
      var iter = 0
      while(readCnt < 8 && iter < 100) {
        iter += 1
        for(i <- 0 until NUM_MEMORY_BANKS) {
          pokeAddr(dut, i, i + (writeCnt*8))
        }
        dut.clock.step()
        if(dut.io.in.ready.peek.litToBoolean) {
          writeCnt += 1
        }
        if(dut.io.out.valid.peek.litToBoolean) {
          for(i <- 0 until NUM_MEMORY_BANKS) {
            expectValue(dut, i, i + readCnt*8)
          }
          readCnt += 1
        }
      }
      assert(readCnt == 8)
      assert(writeCnt >= 8)
    }
  }

  "Address generator and memory" should "read correctly when indices are reversed" in {
    SIMULATION = true
    test(new AddrGenMem(8, "src/test/scala/memory/membankinit")) { dut =>
      dut.io.in.valid.poke(true.B)
      dut.io.out.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.out.valid.expect(false.B)

      var readCnt = 0 //Number of times we've read data
      var writeCnt = 0 //Number of times we've written new addresses to
      var iter = 0
      while(readCnt < 8 && iter < 100) {
        iter += 1
        for(i <- 0 until NUM_MEMORY_BANKS) {
          pokeAddr(dut, i, (NUM_MEMORY_BANKS-1-i) + (writeCnt*8)) //Indices are written at (0) := 7, (1) := 6 etc
        }
        dut.clock.step()
        if(dut.io.in.ready.peek.litToBoolean) {
          writeCnt += 1
        }
        if(dut.io.out.valid.peek.litToBoolean) {
          for(i <- 0 until NUM_MEMORY_BANKS) {
            expectValue(dut, i, i + readCnt*8) //But they are still read out at the correct locations
          }
          readCnt += 1
        }
      }
      assert(readCnt == 8)
      assert(writeCnt >= 8)
    }
  }
}

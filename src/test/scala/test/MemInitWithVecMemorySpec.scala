package test

import chisel3._


import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class MemInitWithVecMemorySpec extends AnyFlatSpec with ChiselScalatestTester with Matchers  {
  behavior of "Fake vector memory"

  it should "store and read the same data" in {
    val width = 4
    val depth = 24
    val bits = 48
    test(new MemInitWithVecMemory(width, depth, bits)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.wrAddr.poke(0.U)
      dut.io.rdAddr.poke(0.U)
      dut.io.we.poke(true.B)
      for(i <- 0 until depth) {
        dut.io.wrData(i).poke(i.S)
      }
      var exp = 0
      for(i <- 0 until depth) {
        exp |= i << (i*bits)
      }
      dut.clock.step()
      for(i <- 0 until 4) {
        dut.io.rdData(i).expect(i.S)
      }
    }
  }

  it should "read stored data" in {
    val width = 32
    val depth = 32
    val bits = 33
    test(new MemInitWithVecMemory(width, depth, bits)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.we.poke(false.B)
      for(w <- 0 until width) {
        dut.io.rdAddr.poke(w.U)
        dut.clock.step()
        for(d <- 0 until depth) {
          //Need to do some magic to parse it as a signed value
          val e = (w*depth+d).toLong
          val E = if(e >= math.pow(2,bits-1)) e-math.pow(2,bits).toLong else e
          print(s"$E ")
          dut.io.rdData(d).expect(E.S)
        }
        println()
      }

    }
  }
}

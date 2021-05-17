package memory

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class IJKgeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  def expectIJK(dut: IJKgenerator, i: Int, j: Int, k: Int): Unit = {
    dut.io.out.i.expect(i.U)
    dut.io.out.j.expect(j.U)
    dut.io.out.k.expect(k.U)
  }

  def pokeIJK(dut: IJKgenerator, i: Int, j: Int, k: Int): Unit = {
    dut.io.in.i.poke(i.U)
    dut.io.in.j.poke(j.U)
    dut.io.in.k.poke(k.U)
  }

  "IJK generator" should "output a vcd file" in {
    test(new IJKgenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(400)
    }
  }

  "IJK generator" should "saturate once invalid" in {
    test(new IJKgenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.ready.poke(true.B)
      while(dut.io.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      expectIJK(dut, 0,0,0)
      dut.io.out.iteration.expect(8.U)

      pokeIJK(dut, 0,0,0)
      dut.io.in.iteration.poke(0.U)
      dut.io.load.poke(true.B)
      dut.clock.step()
      expectIJK(dut, 0,0,0)
      dut.io.out.iteration.expect(0.U)
    }
  }

  "IJK generator" should "load new values" in {
    test(new IJKgenerator) {dut =>
      expectIJK(dut, 0,0,0)
      dut.io.valid.expect(true.B)
      dut.io.ready.poke(true.B)
      dut.clock.step()

      expectIJK(dut, 0,2,0)
      dut.io.valid.expect(true.B)
      dut.io.load.poke(true.B)
      pokeIJK(dut, 1,1,1)

      dut.clock.step()
      expectIJK(dut, 1,1,1)
      dut.io.load.poke(false.B)
      dut.io.ready.poke(false.B)

      dut.clock.step()
      expectIJK(dut, 1,1,1)
    }
  }

  "IJK generator" should "reassert valid when restarted" in {
    test(new IJKgenerator) { dut =>
      pokeIJK(dut, 5,1,5)
      dut.io.in.iteration.poke(7.U)
      dut.io.load.poke(true.B)
      dut.clock.step()

      expectIJK(dut, 5,1,5)
      dut.io.out.iteration.expect(7.U)
      dut.io.load.poke(false.B)
      dut.io.ready.poke(true.B)
      while(dut.io.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      dut.io.ready.poke(false.B)
      dut.io.restart.poke(true.B)
      dut.clock.step()

      expectIJK(dut, 5,1,5)
      dut.io.out.iteration.expect(7.U)
      dut.io.valid.expect(true.B)

    }
  }

  "IJK generator" should "restart to loaded values" in {
    test(new IJKgenerator) {dut =>
      pokeIJK(dut, 1,1,1)
      dut.io.load.poke(true.B)
      dut.clock.step()

      dut.io.load.poke(false.B)
      dut.io.ready.poke(true.B)
      dut.clock.step(2)

      expectIJK(dut, 1,5,1)
      dut.io.restart.poke(true.B)
      dut.clock.step()
      expectIJK(dut, 1,1,1)
    }
  }
}

package memory

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import pipeline.StypeMod
import utils.Config.{NUM_MEMORY_BANKS, NELX, NELY, NELZ}

class IndexGeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  def pokeIJK(dut: IndexGenerator, index: Int, i: Int, j: Int, k: Int): Unit = {
    dut.io.in.bits.ijk(index).poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
    dut.io.in.bits.validIjk(index).poke(true.B)
  }


  def expectIJK(dut: IndexGenerator, index: Int, i: Int, j: Int, k: Int): Unit = {
    val e = i * NELY * NELZ + k * NELY + j
    dut.io.addrGen.bits.indices(index).expect(e.U)
    dut.io.addrGen.bits.validIndices(index).expect(true.B)
  }

  "Index generator" should "correctly generate indices when not pipelined" in {
    test(new IndexGenerator(pipe=false)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.addrGen.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.in.valid.poke(true.B)

      pokeIJK(dut, 0, 0, 0, 0)
      pokeIJK(dut, 1, 1, 0, 0)
      pokeIJK(dut, 2, 2, 0, 0)
      for(i <- 3 until NUM_MEMORY_BANKS) {
        dut.io.in.bits.validIjk(i).poke(false.B)
      }
      dut.clock.step()
      expectIJK(dut, 0, 0, 0, 0)
      expectIJK(dut, 1, 1, 0, 0)
      expectIJK(dut, 2, 2, 0, 0)
      dut.io.addrGen.valid.expect(true.B)





    }
  }
}

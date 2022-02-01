package memory

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import utils.Fixed._
import execution.StypeMod
import utils.Config.{NUM_MEMORY_BANKS, NELX, NELY, NELZ}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class IndexGeneratorSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Index generator"

  val NELXH = (NELX+1)/2
  val NELYH = (NELY+1)/2
  val NELZH = (NELZ+1)/2

  def pokeIJK(dut: IndexGenerator, index: Int, i: Int, j: Int, k: Int): Unit = {
    dut.io.in.bits.ijk(index).poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
    dut.io.in.bits.validIjk(index).poke(true.B)
  }

  def expectIJK(dut: IndexGenerator, index: Int, i: Int, j: Int, k: Int, valid: Boolean = true): Unit = {
    //multiplying by NUM_MEMORY_BANKS instead of bitshifting by three
    val e = (i/2*NELYH*NELZH + k/2*NELYH + j/2)*NUM_MEMORY_BANKS + iterationFromIJK(Array(i,j,k))
    dut.io.addrGen.bits.indices(index).expect(e.U)
    dut.io.addrGen.bits.validIndices(index).expect(valid.B)
  }

  it should "correctly generate indices" in {
    def testFun(dut: IndexGenerator): Unit = {
      dut.io.addrGen.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.io.in.valid.poke(true.B)

      pokeIJK(dut, 0, 0, 0, 0)
      pokeIJK(dut, 1, 1, 0, 0)
      pokeIJK(dut, 2, 2, 0, 0)
      dut.clock.step()
      expectIJK(dut, 0, 0, 0, 0)
      expectIJK(dut, 1, 1, 0, 0)
      expectIJK(dut, 2, 2, 0, 0)
      dut.io.addrGen.valid.expect(true.B)
    }
    test(new IndexGenerator(pipe=true)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testFun(dut)
    }
    test(new IndexGenerator(pipe=false)) { dut =>
      testFun(dut)
    }
  }

  it should "use ready/valid signalling when pipelined" in {
    test(new IndexGenerator(pipe=true)) { dut =>
      //Poke values, but make the consumer non-ready
      dut.io.in.valid.poke(true.B)
      dut.io.addrGen.ready.poke(false.B)
      dut.io.in.ready.expect(true.B)
      dut.io.addrGen.valid.expect(false.B)
      pokeIJK(dut, 0, 0, 0, 0)
      pokeIJK(dut, 1, 1, 0, 0)
      pokeIJK(dut, 2, 2, 0, 0)
      dut.clock.step()
      dut.io.addrGen.valid.expect(true.B)
      dut.io.in.ready.expect(false.B)

      //Poke new values, expect the old values to stay
      pokeIJK(dut, 0, 0, 1, 1)
      pokeIJK(dut, 1, 1, 1, 1)
      pokeIJK(dut, 2, 2, 1, 1)
      dut.clock.step()
      dut.io.addrGen.valid.expect(true.B)
      dut.io.in.ready.expect(false.B)
      expectIJK(dut, 0, 0, 0, 0)
      expectIJK(dut, 1, 1, 0, 0)
      expectIJK(dut, 2, 2, 0, 0)

      //Make the consumer ready
      dut.io.addrGen.ready.poke(true.B)
      dut.io.in.ready.expect(true.B)
      dut.clock.step()

      //new values should now be output
      expectIJK(dut, 0, 0, 1, 1)
      expectIJK(dut, 1, 1, 1, 1)
      expectIJK(dut, 2, 2, 1, 1)
      dut.io.addrGen.valid.expect(true.B)
      dut.io.in.valid.poke(false.B)
      pokeIJK(dut, 1, 2, 3, 4)
      dut.clock.step()

      //Should no longer be valid, should keep old values
      dut.io.addrGen.valid.expect(false.B)
      expectIJK(dut, 0, 0, 1, 1)
      expectIJK(dut, 1, 1, 1, 1)
      expectIJK(dut, 2, 2, 1, 1)
    }
  }

  it should "override validIndices when outside the legal range" in {
    test(new IndexGenerator(pipe=false)) {dut =>
      dut.io.addrGen.ready.poke(true.B)
      dut.io.in.valid.poke(true.B)

      //Whenever i>=NELX, j>=NELY or k>=NELZ, the output should be set invalid
      pokeIJK(dut, 0, 0, 0, NELZ)
      pokeIJK(dut, 1, 0, NELY, 0)
      pokeIJK(dut, 2, 0, NELY, NELZ)
      dut.clock.step()

      for(i <- 0 until 2) {
        dut.io.addrGen.bits.validIndices(i).expect(false.B)
      }
      pokeIJK(dut, 0, NELX, 0, 0)
      pokeIJK(dut, 1, NELX, 0, NELZ)
      pokeIJK(dut ,2, NELX, NELY, 0)
      dut.clock.step()
      for(i <- 0 until 2) {
        dut.io.addrGen.bits.validIndices(i).expect(false.B)
      }

      pokeIJK(dut, 0, NELX, NELY, NELZ)
      pokeIJK(dut, 1, NELX-1, NELY-1, NELZ-1)
      pokeIJK(dut, 2, 0, 0, 0)
      dut.clock.step()
      dut.io.addrGen.bits.validIndices(0).expect(false.B)
      dut.io.addrGen.bits.validIndices(1).expect(true.B)
      dut.io.addrGen.bits.validIndices(2).expect(true.B)
    }
  }
}

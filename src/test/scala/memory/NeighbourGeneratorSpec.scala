package memory

import chisel3._
import chisel3.util._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import utils.Fixed._
import execution.StypeMod
import utils.Config.NUM_MEMORY_BANKS
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class NeighbourGeneratorSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Neighbour generator"

  def expectIJK(dut: NeighbourGenerator, index: Int, i: Int, j: Int, k: Int): Unit = {
    dut.io.indexGen.bits.ijk(index).i.expect(i.U)
    dut.io.indexGen.bits.ijk(index).j.expect(j.U)
    dut.io.indexGen.bits.ijk(index).k.expect(k.U)
    dut.io.indexGen.bits.validIjk(index).expect(true.B)
  }

  it should "forward the input when ELEM is received" in {
    test(new NeighbourGenerator) {dut =>
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.mod.poke(StypeMod.ELEM)

      val ijk1 = (new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U)
      val ijk2 = (new IJKBundle).Lit(_.i -> 0.U, _.j -> 2.U, _.k -> 0.U)
      val ijk3 = (new IJKBundle).Lit(_.i -> 0.U, _.j -> 4.U, _.k -> 0.U)

      dut.io.in.bits.ijk.poke(ijk1)
      dut.io.in.valid.poke(true.B)

      dut.clock.step()
      expectIJK(dut, 0, 0, 0, 0)
      dut.io.in.bits.ijk.poke(ijk2)
      dut.clock.step()
      expectIJK(dut, 0, 0, 2, 0)
      dut.io.in.bits.ijk.poke(ijk3)
      dut.clock.step()
      expectIJK(dut, 0, 0, 4, 0)
    }
  }

  "Neighbour generator" should "output face neighbours" in {
    test(new NeighbourGenerator) {dut =>
      val i = 1
      val j = 2
      val k = 3
      //Setup values
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
      dut.io.in.bits.mod.poke(StypeMod.FCN)
      dut.clock.step()

      //Expect first round of face neighbours
      //Should be at (x,y,z+1), (x,y+1,z) and (x+1,y,z)
      expectIJK(dut, 0, i, j, k+1)
      expectIJK(dut, 1, i, j+1, k)
      expectIJK(dut, 2, i+1, j, k)
      dut.io.in.valid.poke(false.B)
      dut.io.indexGen.valid.expect(true.B)
      dut.clock.step()

      //Expect next round
      //Should be at (x,y,z-1), (x,y-1,z), (x-1,y,z)
      expectIJK(dut, 0, i, j, k-1)
      expectIJK(dut, 1, i, j-1, k)
      expectIJK(dut, 2, i-1, j, k)
      dut.io.indexGen.valid.expect(true.B)
      dut.io.in.valid.poke(false.B)
      dut.clock.step()

      //Expect all 0's and output invalid
      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
        dut.io.indexGen.bits.ijk(i).expect((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U))
      }
      dut.io.indexGen.valid.expect(false.B)
    }
  }

  "Neighbour generator" should "output first set of edge neighbours" in {
    test(new NeighbourGenerator) {dut =>
      val i = 1
      val j = 2
      val k = 3
      //Setup values
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
      dut.io.in.bits.mod.poke(StypeMod.EDN1)
      dut.clock.step()

      //Expect first round of face neighbours
      //Should be at (x,y+1,z+1), (x+1,y,z+1) and (x+1,y+1,z)
      expectIJK(dut, 0, i, j+1, k+1)
      expectIJK(dut, 1, i+1, j, k+1)
      expectIJK(dut, 2, i+1, j+1, k)
      dut.io.indexGen.valid.expect(true.B)
      dut.io.in.valid.poke(false.B)
      dut.clock.step()

      //Expect next round
      //Should be at (x,y-1,z+1), (x-1,y,z+1), (x+1,y-1,z)
      expectIJK(dut, 0, i, j-1, k+1)
      expectIJK(dut, 1, i-1, j, k+1)
      expectIJK(dut, 2, i+1, j-1, k)
      dut.io.indexGen.valid.expect(true.B)
      dut.clock.step()

      //Expect all 0's and output invalid
      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
        dut.io.indexGen.bits.ijk(i).expect((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U))
      }
      dut.io.indexGen.valid.expect(false.B)
    }
  }

  "Neighbour generator" should "output second set of edge neighbours" in {
    test(new NeighbourGenerator) {dut =>
      val i = 1
      val j = 2
      val k = 3
      //Setup values
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
      dut.io.in.bits.mod.poke(StypeMod.EDN2)
      dut.clock.step()

      //Expect first round of face neighbours
      //Should be at (x,y+1,z-1), (x+1,y,z-1) and (x-1,y+1,z)
      expectIJK(dut, 0, i, j+1, k-1)
      expectIJK(dut, 1, i+1, j, k-1)
      expectIJK(dut, 2, i-1, j+1, k)
      dut.io.indexGen.valid.expect(true.B)
      dut.io.in.valid.poke(false.B)
      dut.clock.step()

      //Expect next round
      //Should be at (x,y-1,z-1), (x-1,y,z-1), (x,y-1,z)
      expectIJK(dut, 0, i, j-1, k-1)
      expectIJK(dut, 1, i-1, j, k-1)
      expectIJK(dut, 2, i-1, j-1, k)
      dut.io.indexGen.valid.expect(true.B)
      dut.clock.step()

      //Expect all 0's and output invalid
      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
        dut.io.indexGen.bits.ijk(i).expect((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U))
      }
      dut.io.indexGen.valid.expect(false.B)
    }
  }

  "Neighbour generator" should "output itself as the single element" in {
    test(new NeighbourGenerator) {dut =>
      val i = 1
      val j = 2
      val k = 3
      //Setup values
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> i.U, _.j -> j.U, _.k -> k.U))
      dut.io.in.bits.mod.poke(StypeMod.SEL)
      dut.clock.step()

      //Expect itself
      expectIJK(dut, 0, i, j, k)
      dut.io.indexGen.valid.expect(true.B)
      for(i <- 1 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
      }
      dut.io.in.valid.poke(false.B)
      dut.clock.step()

      //Expect all 0's and output invalid
      dut.io.indexGen.valid.expect(false.B)
      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
        dut.io.indexGen.bits.ijk(i).expect((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U))
      }

    }
  }

  "Neighbour generator" should "test what happens when on an edge/corner" in {
    test(new NeighbourGenerator) { dut =>
      //Setup values
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U))
      dut.io.in.bits.mod.poke(StypeMod.FCN)
      dut.clock.step()

      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        print(s"[$i] ${dut.io.indexGen.bits.ijk(i).peek()}\n")
      }
      dut.clock.step()
      println()
      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        print(s"[$i] ${dut.io.indexGen.bits.ijk(i).peek()}\n")
      }
    }
  }

  "Neighbour generator" should "invalidate inputs when pad is asserted" in {
    test(new NeighbourGenerator) {dut =>
      dut.io.in.ready.expect(true.B)
      dut.io.indexGen.valid.expect(false.B)
      dut.io.in.valid.poke(true.B)
      dut.io.indexGen.ready.poke(true.B)
      dut.io.in.bits.ijk.poke((new IJKBundle).Lit(_.i -> 1.U, _.j -> 1.U, _.k -> 1.U))
      dut.io.in.bits.mod.poke(StypeMod.FCN)
      dut.io.in.bits.pad.poke(true.B)
      dut.clock.step()

      for(i <- 0 until dut.NUM_OUTPUT_PORTS) {
        dut.io.indexGen.bits.validIjk(i).expect(false.B)
      }
    }
  }
}

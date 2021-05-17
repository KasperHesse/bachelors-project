package memory
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation


class EdofGeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Element DOF generator"

  /**
   * Tests whether the element DOF generator follows the same logic as the c-code and outputs values correctly
   * @param dut
   */
  def edofGenTest(dut: EdofGenerator): Unit = {
    val x=3; val y=2; val z=1;
    val edof = getEdof(x,y,z)
    dut.io.prod.bits.i.poke(x.U)
    dut.io.prod.bits.j.poke(y.U)
    dut.io.prod.bits.k.poke(z.U)
    dut.io.prod.valid.poke(true.B)

    dut.clock.step() //latch in values
    dut.io.cons.valid.expect(false.B)

    dut.clock.step() //Move to output stage
    dut.io.cons.valid.expect(true.B)

    for(i <- 0 until 8) {
      dut.io.cons.bits.indices(i).expect(edof(i*3).U)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.cons.bits.indices(i).expect(edof(i*3+1).U)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.cons.bits.indices(i).expect(edof(i*3+2).U)
    }
  }

  /**
   * Tests whether ready/valid signals are asserted properly. Uses hardcoded timing, so don't rely too much on it
   * @param dut
   */
  def testReadyValid(dut: EdofGenerator): Unit = {
    val in = dut.io.prod
    val out = dut.io.cons
    in.bits.i.poke(0.U)
    in.bits.j.poke(1.U)
    in.bits.k.poke(2.U)
    in.valid.poke(true.B)
    //Should be ready but not valid
    out.ready.expect(true.B)
    out.valid.expect(false.B)

    dut.clock.step()
    //Should still be ready, still not valid. Outputstep=0
    out.ready.expect(true.B)
    out.valid.expect(false.B)
    dut.clock.step()
    //Should not be ready, should be valid. Outputstep = 0
    out.ready.expect(false.B)
    out.valid.expect(true.B)
    dut.clock.step()
    //outputstep = 1
    out.ready.expect(false.B)
    out.valid.expect(true.B)
    dut.clock.step()
    //Should still be valid, should also be ready. Outputstep = 2
    out.ready.expect(true.B)
    out.valid.expect(true.B)
    dut.clock.step()
    //Still valid, should not be ready anymore. Outputstep=0
    out.ready.expect(false.B)
    out.valid.expect(true.B)
    in.valid.poke(false.B)
    dut.clock.step(2) //outputstep = 2
    out.ready.expect(true.B)
    out.valid.expect(true.B)
    dut.clock.step(3) //outputstep 2, second time around
    out.ready.expect(true.B)
    out.valid.expect(true.B)
    dut.clock.step() //outputstep=0
    out.ready.expect(true.B)
    out.valid.expect(false.B)
    dut.clock.step(5) //outputstep=0
    out.ready.expect(true.B)
    out.valid.expect(false.B)



  }

  /**
   * Compute the indices of the of the 24 degrees of freedom associated with the 8 corners of the element at (i,j,k) in the grid.
   * Coden taken from top.c and ported to Scala
   *
   * @param i Current iteration/coordinate in the x-direction
   * @param j Current iteration/coordinate in the y-direction
   * @param k Current iteration/coordinate in the z-direction
   */
  def getEdof(i: Int, j: Int, k: Int): Array[Int] = {
    val edof = Array.ofDim[Int](24)
    val nx_1 = i
    val nx_2 = i+1
    val ny_1 = j
    val ny_2 = j + 1
    val nz_1 = k
    val nz_2 = k + 1

    val nIndex: Array[Int] = Array.fill(8)(0)

    //For (i,j,k)=(0,0,0)
    //index1=0+0+1=1,		index2=16+0+1=17,		index3=16+0+0=16,		index4=0+0+0=0,
    //index5=0+4+1=5,		index6=16+4+1=21,		index7=16+4+0=20,		index8=0+4+0=4
    nIndex(0) = nx_1 * NY * NZ + nz_1 * NY + ny_2;
    nIndex(1) = nx_2 * NY * NZ + nz_1 * NY + ny_2;
    nIndex(2)= nx_2 * NY * NZ + nz_1 * NY + ny_1;
    nIndex(3) = nx_1 * NY * NZ + nz_1 * NY + ny_1;
    nIndex(4) = nx_1 * NY * NZ + nz_2 * NY + ny_2;
    nIndex(5) = nx_2 * NY * NZ + nz_2 * NY + ny_2;
    nIndex(6) = nx_2 * NY * NZ + nz_2 * NY + ny_1;
    nIndex(7) = nx_1 * NY * NZ + nz_2 * NY + ny_1;

    //nIndex(0) => 0, 8, 16
    //nIndex(1) => 1, 9, 17

    for(i <- 0 until 8) {
      for(j <- 0 until 3) {
        edof(j*8 + i) = 3*nIndex(i) + j
      }
    }

//
//    edof(0) = 3 * nIndex1 + 0;
//    edof(1) = 3 * nIndex1 + 1;
//    edof(2) = 3 * nIndex1 + 2;
//    edof(3) = 3 * nIndex2 + 0;
//    edof(4) = 3 * nIndex2 + 1;
//    edof(5) = 3 * nIndex2 + 2;
//    edof(6) = 3 * nIndex3 + 0;
//    edof(7) = 3 * nIndex3 + 1;
//    edof(8) = 3 * nIndex3 + 2;
//    edof(9) = 3 * nIndex4 + 0;
//    edof(10) = 3 * nIndex4 + 1;
//    edof(11) = 3 * nIndex4 + 2;
//    edof(12) = 3 * nIndex5 + 0;
//    edof(13) = 3 * nIndex5 + 1;
//    edof(14) = 3 * nIndex5 + 2;
//    edof(15) = 3 * nIndex6 + 0;
//    edof(16) = 3 * nIndex6 + 1;
//    edof(17) = 3 * nIndex6 + 2;
//    edof(18) = 3 * nIndex7 + 0;
//    edof(19) = 3 * nIndex7 + 1;
//    edof(20) = 3 * nIndex7 + 2;
//    edof(21) = 3 * nIndex8 + 0;
//    edof(22) = 3 * nIndex8 + 1;
//    edof(23) = 3 * nIndex8 + 2;
    edof

    /*

     */

    //Need to reorder these bitches ...
  }

  def pokeIJK(dut: EdofGenerator, i: Int, j: Int, k: Int): Unit = {
    dut.io.prod.bits.i.poke(i.U)
    dut.io.prod.bits.j.poke(j.U)
    dut.io.prod.bits.k.poke(k.U)
  }

  def expectIndices(dut: EdofGenerator, indices: Array[Int]): Unit = {
    for(i <- indices.indices) {
      dut.io.cons.bits.indices(i).expect(indices(i).U)
    }
  }

  "EDOF generator" should "only deassert ready when valid is asserted" in {
    test(new EdofGenerator) {dut =>
      dut.io.prod.valid.poke(false.B)
      for(i <- 0 until 5) {
        dut.io.prod.ready.expect(true.B)
        dut.io.cons.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.prod.valid.poke(true.B)
      dut.clock.step()
      dut.io.cons.valid.expect(true.B)
      dut.io.prod.ready.expect(false.B)
    }
  }

  "EDOF generator" should "change output values when ready is asserted" in {
    test(new EdofGenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val edof = getEdof(1,2,3)

      dut.io.prod.valid.poke(true.B)
      pokeIJK(dut, 1,2,3)
      dut.clock.step()

      //Observe outputs for 2 clock cycles when consumer is not ready
      for(i <- 0 until 3) {
        expectIndices(dut, edof.slice(0,8))
        dut.clock.step()
      }
      dut.io.prod.ready.expect(false.B)
      dut.io.cons.ready.poke(true.B)
      dut.clock.step()

      //Step through outputs when consumer is ready
      expectIndices(dut, edof.slice(8, 16))
      dut.io.prod.ready.expect(false.B)
      dut.clock.step()

      expectIndices(dut, edof.slice(16, 24))
      //Verify that producer ready-signal is combinationally coupled to consumer ready-signal
      dut.io.prod.ready.expect(true.B)
      dut.io.cons.ready.poke(false.B)
      dut.io.prod.ready.expect(false.B)
    }
  }

}

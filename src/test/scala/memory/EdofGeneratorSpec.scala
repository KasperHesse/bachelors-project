package memory
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
class EdofGeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Element DOF generator"

  /**
   * Tests whether the element DOF generator follows the same logic as the c-code and outputs values correctly
   * @param dut
   * @param ny
   * @param nz
   */
  def edofGenTest(dut: EdofGenerator, ny: Int, nz: Int): Unit = {
    val x=3; val y=2; val z=1;
    val edof = getEdof(x,y,z,ny,nz)
    dut.io.in.i.poke(x.U)
    dut.io.in.j.poke(y.U)
    dut.io.in.k.poke(z.U)
    dut.io.in.valid.poke(true.B)

    dut.clock.step() //latch in values
    dut.io.out.valid.expect(false.B)

    dut.clock.step() //Move to output stage
    dut.io.out.valid.expect(true.B)

    for(i <- 0 until 8) {
      dut.io.out.offsets(i).expect(edof(i*3).U)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.out.offsets(i).expect(edof(i*3+1).U)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.out.offsets(i).expect(edof(i*3+2).U)
    }
  }

  /**
   * Tests whether ready/valid signals are asserted properly. Uses hardcoded timing, so don't rely too much on it
   * @param dut
   */
  def testReadyValid(dut: EdofGenerator): Unit = {
    val in = dut.io.in
    val out = dut.io.out
    in.i.poke(0.U)
    in.j.poke(1.U)
    in.k.poke(2.U)
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
   * @param ny Number of nodes in the y-direction of the grid
   * @param nz Number of nodes in the x-direction of the grid
   */
  def getEdof(i: Int, j: Int, k: Int, ny: Int, nz: Int): Array[Int] = {
    val edof = Array.ofDim[Int](24)
    val nx_1 = i
    val nx_2 = i+1
    val ny_1 = j
    val ny_2 = j + 1
    val nz_1 = k
    val nz_2 = k + 1

    //index1=0+0+1=1,		index2=16+0+1=17,		index3=16+0+0=16,		index4=0+0+0=0,
    //index5=0+4+1=5,		index6=16+4+1=21,		index7=16+4+0=20,		index8=0+4+0=4
    val nIndex1 = nx_1 * ny * nz + nz_1 * ny + ny_2;
    val nIndex2 = nx_2 * ny * nz + nz_1 * ny + ny_2;
    val nIndex3 = nx_2 * ny * nz + nz_1 * ny + ny_1;
    val nIndex4 = nx_1 * ny * nz + nz_1 * ny + ny_1;
    val nIndex5 = nx_1 * ny * nz + nz_2 * ny + ny_2;
    val nIndex6 = nx_2 * ny * nz + nz_2 * ny + ny_2;
    val nIndex7 = nx_2 * ny * nz + nz_2 * ny + ny_1;
    val nIndex8 = nx_1 * ny * nz + nz_2 * ny + ny_1;

    edof(0) = 3 * nIndex1 + 0;
    edof(1) = 3 * nIndex1 + 1;
    edof(2) = 3 * nIndex1 + 2;
    edof(3) = 3 * nIndex2 + 0;
    edof(4) = 3 * nIndex2 + 1;
    edof(5) = 3 * nIndex2 + 2;
    edof(6) = 3 * nIndex3 + 0;
    edof(7) = 3 * nIndex3 + 1;
    edof(8) = 3 * nIndex3 + 2;
    edof(9) = 3 * nIndex4 + 0;
    edof(10) = 3 * nIndex4 + 1;
    edof(11) = 3 * nIndex4 + 2;
    edof(12) = 3 * nIndex5 + 0;
    edof(13) = 3 * nIndex5 + 1;
    edof(14) = 3 * nIndex5 + 2;
    edof(15) = 3 * nIndex6 + 0;
    edof(16) = 3 * nIndex6 + 1;
    edof(17) = 3 * nIndex6 + 2;
    edof(18) = 3 * nIndex7 + 0;
    edof(19) = 3 * nIndex7 + 1;
    edof(20) = 3 * nIndex7 + 2;
    edof(21) = 3 * nIndex8 + 0;
    edof(22) = 3 * nIndex8 + 1;
    edof(23) = 3 * nIndex8 + 2;

    edof
  }

  it should "test whether EDOF generation works correctly" in {
    val nx = 6
    val ny = 6
    val nz = 6
    test(new EdofGenerator(nx,ny,nz, 8)) {c =>
      edofGenTest(c,ny,nz)
    }
  }

  it should "correctly assert ready/valid signals" in {
    val nx = 6
    val ny = 6
    val nz = 6
    test(new EdofGenerator(nx,ny,nz, 8)) {c =>
      testReadyValid(c)
    }
  }
}

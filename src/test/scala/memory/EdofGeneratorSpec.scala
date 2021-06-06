package memory
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import pipeline.StypeMod._
import pipeline.{StypeMod, seed}


class EdofGeneratorSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "EDOF generator"

  /**
   * Tests whether the element DOF generator follows the expected logic and outputs correctly.
   * Will generate a random ijk-value, and will also randomize the values of NX, NY and NZ to ensure that
   * everything works as expected every t ime
   * @param dut
   */
  def edofGenTest(dut: EdofGenerator): Unit = {
    val rand = scala.util.Random
    val i = rand.nextInt(NELX)
    val j = rand.nextInt(NELY)
    val k = rand.nextInt(NELZ)
    val edof = getEdof(i,j,k)

    println(s"NELX: $NELX.  NELY: $NELY.  NELZ: $NELZ. (i,j,k)=($i,$j,$k)")
    dut.io.in.bits.ijk.i.poke(i.U)
    dut.io.in.bits.ijk.j.poke(j.U)
    dut.io.in.bits.ijk.k.poke(k.U)
    dut.io.in.valid.poke(true.B)
    dut.io.addrGen.ready.poke(true.B)
    dut.io.in.bits.mod.poke(StypeMod.DOF)
    dut.io.in.ready.expect(true.B)


    dut.clock.step() //latch in values
    dut.io.in.ready.expect(false.B)
    dut.io.addrGen.valid.expect(true.B)

    for(i <- 0 until 8) {
      dut.io.addrGen.bits.indices(i).expect(edof(i).U)
      dut.io.addrGen.bits.validIndices(i).expect(true.B)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.addrGen.bits.indices(i).expect(edof(i+8).U)
      dut.io.addrGen.bits.validIndices(i).expect(true.B)
    }
    dut.clock.step()
    for(i <- 0 until 8) {
      dut.io.addrGen.bits.indices(i).expect(edof(i+16).U)
      dut.io.addrGen.bits.validIndices(i).expect(true.B)
    }
  }


  def pokeIJK(dut: EdofGenerator, i: Int, j: Int, k: Int): Unit = {
    dut.io.in.bits.ijk.i.poke(i.U)
    dut.io.in.bits.ijk.j.poke(j.U)
    dut.io.in.bits.ijk.k.poke(k.U)
  }

  def expectIndices(dut: EdofGenerator, indices: Array[Int]): Unit = {
    for(i <- indices.indices) {
      dut.io.addrGen.bits.indices(i).expect(indices(i).U)
    }
  }

  it should "test output generation values" in {
    seed("Edof generator random NX; NY; NZ values")
    val rand = scala.util.Random
    NELX = rand.nextInt(6) + 4
    NELY = rand.nextInt(6) + 4
    NELZ = rand.nextInt(6) + 4
    NX = NELX + 1
    NY = NELY + 1
    NZ = NELZ + 1
    test(new EdofGenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      edofGenTest(dut)
    }
  }

  it should "deassert some valid signals when mod is fdof" in {
    seed("Edof generator, fixed dof")
    test(new EdofGenerator) {dut =>
      dut.io.in.initSource().setSourceClock(dut.clock)
      dut.io.addrGen.ready.poke(true.B)
      val ijk = Array(0,0,0,0)
      val instr = genIJKinput(IJK=Some(ijk), pad=false, mod=FDOF)
      dut.io.in.enqueue(instr)
      while(!dut.io.addrGen.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      //Expect lower 4 dof's to be valid, upper 4 should not be valid
      for(i <- 0 until 3) {
        for(i <- 0 until 8) {
          dut.io.addrGen.bits.validIndices(i).expect((i < 4).B)
        }
        dut.io.addrGen.valid.expect(true.B)
        dut.clock.step()
      }
      dut.io.addrGen.valid.expect(false.B)
    }
  }

  it should "deassert all valid signals when mod=fdof and i>=1" in {
    seed("Edof generator, fixed dof with nonzero i")
    test(new EdofGenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.in.initSource().setSourceClock(dut.clock)
      dut.io.addrGen.ready.poke(true.B)
      var ijk: Array[Int] = genIJK()
      while(ijk(0) == 0) {
        ijk = genIJK()
      }
      val instr = genIJKinput(IJK=Some(ijk), pad=false, mod=FDOF)
      dut.io.in.enqueue(instr)
      while(!dut.io.addrGen.valid.peek.litToBoolean) {
        dut.clock.step()
      }
      for(i <- 0 until 3) {
        dut.io.addrGen.bits.validIndices.foreach(_.expect(false.B))
        dut.io.addrGen.valid.expect(true.B)
        dut.clock.step()
      }
      dut.io.addrGen.valid.expect(false.B)
    }
  }

  it should "only deassert ready when valid is asserted" in {
    test(new EdofGenerator) {dut =>
      dut.io.in.valid.poke(false.B)
      for(i <- 0 until 5) {
        dut.io.in.ready.expect(true.B)
        dut.io.addrGen.valid.expect(false.B)
        dut.clock.step()
      }
      dut.io.in.valid.poke(true.B)
      dut.clock.step()
      dut.io.addrGen.valid.expect(true.B)
      dut.io.in.ready.expect(false.B)
    }
  }

  it should "change output values when ready is asserted" in {
    test(new EdofGenerator).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val edof = getEdof(1,2,3)

      dut.io.in.valid.poke(true.B)
      pokeIJK(dut, 1,2,3)
      dut.clock.step()

      //Observe outputs for 2 clock cycles when consumer is not ready
      for(i <- 0 until 3) {
        expectIndices(dut, edof.slice(0,8))
        dut.clock.step()
      }
      dut.io.in.ready.expect(false.B)
      dut.io.addrGen.ready.poke(true.B)
      dut.clock.step()

      //Step through outputs when consumer is ready
      expectIndices(dut, edof.slice(8, 16))
      dut.io.in.ready.expect(false.B)
      dut.clock.step()

      expectIndices(dut, edof.slice(16, 24))
      //Verify that producer ready-signal is combinationally coupled to consumer ready-signal
      dut.io.in.ready.expect(true.B)
      dut.io.addrGen.ready.poke(false.B)
      dut.io.in.ready.expect(false.B)
    }
  }

}

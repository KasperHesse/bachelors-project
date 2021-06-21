package execution

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._
import RegisterFileType._
import Opcode._
import utils.Config

class WritebackSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Writeback stage"

  def genDestination(rf: RegisterFileType.Type, rd: Int, subvec: Int): RegisterBundle = {
    (new RegisterBundle).Lit(_.rf -> rf, _.reg -> rd.U, _.subvec -> subvec.U, _.rfUint -> rf.litValue().U)
  }

  def testVreg(dut: Writeback): Unit = {
    val rand = scala.util.Random
    //Generate inputs
    val inputs = Array.fill(SUBVECTORS_PER_VREG)(Array.fill(NUM_PROCELEM)(rand.nextInt(100).S))
    val rd = rand.nextInt(NUM_VREG)
    val dests = for(i <- 0 until SUBVECTORS_PER_VREG) yield {
      genDestination(VREG, rd, i)
    }

    //Poke em
    for(i <- 0 until SUBVECTORS_PER_VREG) {
      for(j <- 0 until NUM_PROCELEM) {
        dut.io.ex.res(j).poke(inputs(i)(j))
      }
      dut.io.ex.dest.poke(dests(i))
      dut.io.ex.valid.poke(true.B)
      dut.io.ex.reduce.poke(false.B)
      dut.clock.step()
      print(s"i=$i\n")
      if(rand.nextInt(3) == 2 && i != SUBVECTORS_PER_VREG-1) {
        print("\tDeasserting valid to simulate a pipeline stall\n")
        dut.io.ex.valid.poke(false.B)
        dut.clock.step(3)
      }
    }
    for(i <- 0 until VREG_DEPTH) {
      val sv = i/NUM_PROCELEM
      val j = i % NUM_PROCELEM
      dut.io.id.wrData(i).expect(inputs(sv)(j))
      dut.io.id.rd.reg.expect(rd.U)
      dut.io.id.rd.rf.expect(VREG)
    }
  }

  //tests xreg output when the instruction was of the type RedVV
  def testRedVV(dut: Writeback): Unit = {
    //Generate XREG_DEPTH inputs. Each output should be a reduced version of the inputs
    val inputs = Seq.fill(XREG_DEPTH)(Seq.fill(NUM_PROCELEM)(genDouble()))
    val fixedInputs = inputs.map(i => i.map(double2fixed))
    val results = fixedInputs.map(i => i.reduce((a,b) => fixedAdd(a,b)))

    dut.io.ex.valid.poke(true.B)
    val rd = (new RegisterBundle).Lit(_.reg -> 1.U, _.rf -> XREG, _.rfUint -> XREG.litValue.U, _.subvec -> 0.U)
    dut.io.ex.dest.poke(rd)
    dut.io.ex.reduce.poke(true.B)
    for(i <- 0 until XREG_DEPTH) {
      for (j <- 0 until NUM_PROCELEM) {
        dut.io.ex.res(j).poke(fixedInputs(i)(j).S)
      }
      dut.clock.step()
    }
    while(!dut.io.id.we.peek.litToBoolean) {
      dut.clock.step()
    }
    for(i <- results.indices) {
      dut.io.id.wrData(i).expect(results(i).S)
    }
    dut.io.id.rd.expect(rd)

  }

  /**
   * Tests whether the writeback stage correctly presents an output after a single clock cycle
   * @param dut The DUT
   * @param rf The register file type for the result
   * @param reduce Whether the inputs should be add-reduced into a single value (default: false)
   */
  def testSingle(dut: Writeback, rf: RegisterFileType.Type, reduce: Boolean = false): Unit = {
    require(rf != VREG, "Cannot test VREG outputs as single-cycle outputs")
    val rand = scala.util.Random

    //Generate input
    val inputs = Array.fill(NUM_PROCELEM)(rand.nextInt(100).S)
    val rd = rand.nextInt(NUM_VREG)
    val dest = genDestination(rf, rd, 0)

    //Poke inputs
    for (i <- 0 until NUM_PROCELEM) {
      dut.io.ex.res(i).poke(inputs(i))
    }
    dut.io.ex.valid.poke(true.B)
    dut.io.ex.reduce.poke(reduce.B)
    dut.io.ex.dest.poke(dest)
    dut.clock.step()

    //Expect output
    if(reduce) {
      val res = inputs.reduce( (a,b) => fixedAdd(a,b))
      for(i <- 0 until NUM_PROCELEM) {
        dut.io.id.wrData(i).expect(res)
      }
    } else {
      for (i <- 0 until NUM_PROCELEM) {
        dut.io.id.wrData(i).expect(inputs(i))
      }
    }
    for (i <- NUM_PROCELEM until VREG_DEPTH) {
      dut.io.id.wrData(i).expect(0.S)
    }
    dut.io.id.rd.rf.expect(rf)
    dut.io.id.rd.reg.expect(rd.U)
    dut.io.id.we.expect(true.B)
  }

  it should "build an output when rf=VREG" in {
    simulationConfig()
    test(new Writeback).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("Writeback vreg output")
      testVreg(dut)
    }
  }

  it should "display the output when rf=XREG" in {
    simulationConfig()
    test(new Writeback) {dut =>
      testSingle(dut, XREG)
    }
  }

  it should "display the output when rf=SREG" in {
    simulationConfig()
    test(new Writeback) {dut =>
      testSingle(dut, SREG)
    }
  }

  it should "reduce the output when rf=SREG" in {
    simulationConfig()
    test(new Writeback) {dut =>
      testSingle(dut, SREG, true)
    }
  }

  it should "reduce the result on red.vv instructions" in {
    simulationConfig()
    seed("Writeback, red.vv", Some(1L))
    test(new Writeback).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testRedVV(dut)
    }
  }
}

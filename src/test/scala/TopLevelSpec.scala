import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.{Assembler, SynthesisMemInit}
import utils.Fixed._

import scala.io.Source

class TopLevelSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Top Level Module"

  def testFun(filename: String, cycles: Int = 999): Unit = {
    val source = Source.fromFile(f"src/resources/$filename.txt")
    Assembler.writeMemInitFile(f"src/resources/$filename.hex.txt", Assembler.assemble(source))
    source.close()
    FIXED_WIDTH = 26
    INT_WIDTH = 10
    FRAC_WIDTH = 15
    SynthesisMemInit("src/resources/meminit")
    test(new TopLevel(IMsize=128, IMinitFileLocation = f"resources/$filename.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")) {dut =>
      dut.clock.setTimeout(cycles+1)
      dut.clock.step(cycles)
    }
  }

  it should "perform the first half of applyDensityFilterGradient" in {
    testFun("density_filter_gradient_1", 600)
  }

  it should "perform the second half of applyDensityFilterGradient" in {
    testFun("density_filter_gradient_2", 1200)
  }

  it should "perform apply density filter" in {
    testFun("applydensityfilter", 500)
  }

  it should "perform applyStateOperator" in {
    testFun("applyStateOperator", 1400)
  }

  it should "perform an inner product" in {
    testFun("innerproduct", 1000)
  }

  it should "perform a vector norm" in {
    testFun("norm", 700)
  }

  it should "perform generateMatrixDiagonal" in {
    testFun("generateMatrixDiagonal", 800)
  }

  it should "perform preconditionDampedJacobi" in { //NOTE: This should be run with pstart nelemvec instead of pstart nelemdof to reduce runtime. Aint nobody got time for dat
    testFun("preconditionDampedJacobi", 700)
  }

  it should "perform getComplianceAndSensitivity" in {
    testFun("getComplianceAndSensitivity", 1200)
  }

  it should "perform lagrangian update" in {
    testFun("lagrange", 1000)
  }

  it should "perform the second half of lagrangian update" in {
    testFun("lagrange2", 1000)
  }


}

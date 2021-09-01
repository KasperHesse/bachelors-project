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

  it should "perform setup and an elementwise operation" in {
    val source = Source.fromFile("resources/setupandelementwise.txt")
    Assembler.writeMemInitFile("resources/setupandelementwise.hex.txt", Assembler.assemble(source))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    FIXED_WIDTH = 26
    INT_WIDTH = 10
    FRAC_WIDTH = 15
    test(new TopLevel(IMsize=128, IMinitFileLocation = "resources/setupandelementwise.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(600)
    }
  }

  it should "perform setup and the second half of density filter gradient" in {
    val source = Source.fromFile("resources/density_filter_gradient_second_half.txt")
    Assembler.writeMemInitFile("resources/density_filter_gradient_second_half.hex.txt", Assembler.assemble(source))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    FIXED_WIDTH = 26
    INT_WIDTH = 10
    FRAC_WIDTH = 15
    test(new TopLevel(IMsize=128, IMinitFileLocation = "resources/density_filter_gradient_second_half.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.setTimeout(1200)
      dut.clock.step(1200)
    }
  }

  it should "perform a matrix-vector product" in {
    val source = Source.fromFile("resources/mvp.txt")
    Assembler.writeMemInitFile("resources/mvp.hex.txt", Assembler.assemble(source).map(_.toLong))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    test(new TopLevel(IMsize = 128, IMinitFileLocation = "resources/mvp.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.setTimeout(2000)
      dut.clock.step(1500)
    }
  }

  it should "perform the setup before applyStateOperator" in {
    val source = Source.fromFile("resources/asosetup.txt")
    Assembler.writeMemInitFile("resources/asosetup.hex.txt", Assembler.assemble(source).map(_.toLong))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    test(new TopLevel(IMsize = 128, IMinitFileLocation = "resources/asosetup.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(500)
    }
  }

  it should "perform an inner product" in {
    val source = Source.fromFile("resources/innerproduct.txt")
    Assembler.writeMemInitFile("resources/innerproduct.hex.txt", Assembler.assemble(source).map(_.toLong))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    test(new TopLevel(IMsize = 128, IMinitFileLocation = "resources/innerproduct.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(200)
    }
  }

  it should "perform apply density filter" in {
    val source = Source.fromFile("resources/applydensityfilter.txt")
    Assembler.writeMemInitFile("resources/applydensityfilter.hex.txt", Assembler.assemble(source).map(_.toLong))
    source.close()
    SynthesisMemInit("src/resources/meminit")
    test(new TopLevel(IMsize = 128, IMinitFileLocation = "resources/applydensityfilter.hex.txt", wordsPerBank=1671, memInitFileLocation="src/resources/meminit")).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(300)
    }
  }
}

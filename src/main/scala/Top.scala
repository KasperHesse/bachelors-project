import arithmetic._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import memory.MemoryWriteback
import memory._
import execution._
import stages.DecodeTiming
import test._
import utils.{Assembler, SynthesisMemInit}

import scala.io.Source

object Top extends App {
  //Assemble program

  val source = Source.fromFile("src/main/c/topopt/top2.asm")
  Assembler.writeMemInitFile("src/main/c/topopt/top2.hex.txt", Assembler.assemble(source))
  source.close()

  //Initialize starting values in memory for synthesis
  val wordsPerBank = SynthesisMemInit("src/resources/meminit")

  utils.Config.INLINE = true
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new TopLevel(IMsize=1024, IMinitFileLocation = "meminit/top2.hex.txt", wordsPerBank, memInitFileLocation="src/resources/meminit", clkFreq = 50e6.toInt))))
//  -X verilog outputs verilog, -td target/gen sets target directory
}

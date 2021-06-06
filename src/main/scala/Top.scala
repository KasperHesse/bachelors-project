import vector._
import arithmetic._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import memory.MemoryWriteback
import memory._
import pipeline._
import test._
import utils.{Assembler, SynthesisMemInit}

import scala.io.Source

object Top extends App {
  //Assemble program
  val source = Source.fromFile("resources/program.txt")
  Assembler.writeMemInitFile("resources/im.txt", Assembler.assemble(source).map(_.toLong))
  source.close()

  SynthesisMemInit()

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new TopLevel(IMsize=1024, IMinitFileLocation = "resources/im.txt", wordsPerBank=1671, memInitFileLocation = "resources"))))
//  -X verilog outputs verilog, -td target/gen sets target directory
}

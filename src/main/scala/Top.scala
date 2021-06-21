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
  val source = Source.fromFile("src/resources/programs/applystateoperator.txt")
  Assembler.writeMemInitFile("src/resources/programs/applystateoperator.hex", Assembler.assemble(source).map(_.toLong))
  source.close()

  //Initialize starting values in memory for synthesis
//  SynthesisMemInit()

  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new DecodeTiming(50e6.toInt, ""))))
//  (new chisel3.stage.ChiselStage).execute(
//    Array("-X", "verilog", "-td", "target/gen"),
//    Seq(ChiselGeneratorAnnotation(() => new TopLevel(IMsize=1024, IMinitFileLocation = "src/resources/programs/applystateoperator.hex", wordsPerBank=1671, memInitFileLocation="src/resources/memInit"))))
//  -X verilog outputs verilog, -td target/gen sets target directory
}

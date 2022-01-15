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
  //Initialize starting values in memory for synthesis
//  val wordsPerBank = SynthesisMemInit("src/resources/meminit_synth")
//
//  //Assemble the program
  val source = Source.fromFile("src/resources/programs/top.txt")
  Assembler.writeMemInitFile("src/resources/programs/top.hex.txt", Assembler.assemble(source), 8)
  source.close()
//
//  utils.Config.INLINE = true
//  (new chisel3.stage.ChiselStage).execute(
//    Array("-X", "verilog", "-td", "target/gen"),
//    Seq(ChiselGeneratorAnnotation(() => new TopLevel(
//      IMsize=1024,
//      IMinitFileLocation = "src/resources/programs/top.hex.txt",
//      wordsPerBank,
//      memInitFileLocation = "src/resources/meminit_synth",
//      clkFreq=50e6.toInt)
//    )))

//  (new chisel3.stage.ChiselStage).execute(
//    Array("-X", "verilog", "-td", "target/gen"), Seq(ChiselGeneratorAnnotation(() => new UartTransmitter()))
//  )
//  -X verilog outputs verilog, -td target/gen sets target directory
}

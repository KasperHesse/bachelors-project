import chisel3.stage.ChiselStage
import execution.KEMatrix
import utils.{Assembler, SynthesisMemInit, UseUart}

import scala.io.Source

object Top extends App {
  //Initialize starting values in memory for synthesis
  val wordsPerBank = SynthesisMemInit("src/resources/meminit_synth")

  //Assemble the program
  val source = Source.fromFile("src/resources/programs/top.txt")
  Assembler.writeMemInitFile("src/resources/programs/top.hex.txt", Assembler.assemble(source), 8)
  source.close()

  utils.Config.INLINE = true
  (new ChiselStage).emitVerilog(
    new TopLevel(
      IMsize=1024,
      IMinitFileLocation = "../../src/resources/programs/top.hex.txt", //Using ../.. to generate files that Quartus can use without issues
      wordsPerBank,
      memInitFileLocation = "../../src/resources/meminit_synth",
      clkFreq=50e6.toInt),
    Array("-X", "verilog", "-td", "generated", "-e", "verilog"))
  //The KE-matrix generated with TopLevel doesn't contain readmemh-statements for some reason
  //Also generating that one here, to ensure everything is correct
  (new ChiselStage).emitVerilog(new KEMatrix(sync=true, memInitFileLocation = "../../src/resources/ke"), Array("-X", "verilog", "-td", "generated"))

  //-X verilog outputs verilog files
  //-td generated places files into directory "generated"
  //-e verilog generates one file per module
}

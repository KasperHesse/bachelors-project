import vector._
import arithmetic._
import chisel3.stage.{ChiselGeneratorAnnotation, ChiselStage}
import memory.MemoryWriteback
import memory._
import pipeline._
import test.{OnChipMemTest, UsingOnChipMemTest}

object Top extends App {
//  utils.Config.NUM_MEMORY_BANKS = 1
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new pipeline.ThreadV2(0))))
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new OnChipMemTest(true))))
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new OnChipMemTest(false))))
  //-X verilog outputs verilog, -td target/gen sets target directory
}

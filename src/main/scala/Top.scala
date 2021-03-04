import vector._
import arithmetic._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

object Top extends App {
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new FixedPointAddSub())))
  //-X verilog outputs verilog, -td target/gen sets target directory

  //TODO: Add a command to the makefile, so I can add a file to a default project, set it as top,
    //run quartus_map and open the RTL viewer

}

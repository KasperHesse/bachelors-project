import vector._
import arithmetic._
import chisel3.stage.{ChiselStage, ChiselGeneratorAnnotation}

object Top extends App {
  (new chisel3.stage.ChiselStage).execute(
    Array("-X", "verilog", "-td", "target/gen"),
    Seq(ChiselGeneratorAnnotation(() => new test.UsingOnChipMem)))
  //-X verilog outputs verilog, -td target/gen sets target directory

  //TODO: Add a command to the makefile, so I can add a file to a default project, set it as top,
    //run quartus_map and open the RTL viewer

  /*
  TODO: Figure out if we really need an enable signal all the way back to the main unit.
    It probably makes sense to have it inside of the processing elements, but we can probably find a
    smarter way to implement their assertion outside of the MPU (any transition between two states, new state not
    being a NOP should indicate a valid transaction.
    Should still be able to flush the transactions though


   */
}

package pipeline

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import utils.Config._
import firrtl.annotations._

class FetchIO extends Bundle {
  val id = new IfIdIO
//  val addr = Input(UInt(4.W))
}

class Fetch(memsize: Int = 1024, memfile: String = "") extends Module {
  val io = IO(new FetchIO)

  val IP = RegInit(0.U(32.W))
  IP := IP + 4.U

  val imem = Mem(memsize, UInt(32.W))
  if(SIMULATION) {
    require(!memfile.isEmpty, "Cannot simulate without memory init")
    loadMemoryFromFile(imem, memfile)
  }
  io.id.instr := imem((IP >> 2).asUInt())
}

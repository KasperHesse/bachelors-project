package pipeline

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import utils.Config._

import scala.io.Source

class FetchIO extends Bundle {
  val id = new IfIdIO
  val ctrl = new IfControlIO
}

class Fetch(memsize: Int = 1024, memfile: String = "") extends Module {
  val io = IO(new FetchIO)

  val PC: UInt = RegInit(0.U(32.W))
  val imem: Mem[UInt] = Mem(memsize, UInt(32.W))

  when(io.id.branch) {
    PC := io.id.branchTarget
  } .elsewhen(io.ctrl.iload) {
    PC := PC + 4.U
  }
  val instr: UInt = imem((PC >> 2).asUInt())


  if(SIMULATION) {
    require(!memfile.isEmpty, "Cannot simulate with empty memory init file")
    val src = Source.fromFile(memfile)
    require(src.getLines().length <= memsize, s"Memory file (${src.getLines.length}) too large for memory size ($memsize)")
    src.close()
    loadMemoryFromFile(imem, memfile)
  }

  io.id.instr := instr
  io.ctrl.instr := instr
  io.id.pc := PC
}

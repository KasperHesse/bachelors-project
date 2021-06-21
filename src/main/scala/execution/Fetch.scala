package execution

import chisel3._
import chisel3.util.experimental.loadMemoryFromFile
import chisel3.util.experimental.loadMemoryFromFileInline
import utils.Config._

import scala.io.Source

class FetchIO extends Bundle {
  val id = new IfIdIO
  val ctrl = new IfControlIO
}

/**
 * The fetch stage
 * @param memsize The number of instructions stored in instruction memory
 * @param memfile Location of memory initialization file. If none is set, does not emit a readmemh statement.
 */
class Fetch(memsize: Int = 1024, memfile: String = "") extends Module {
  val io = IO(new FetchIO)

  val PC: UInt = RegInit(0.U(32.W))
  val PCnext: UInt = WireDefault(0.U(32.W))
  val imem: Mem[UInt] = Mem(memsize, UInt(INSTRUCTION_WIDTH.W))

  when(io.id.branch) {
    PCnext := io.id.branchTarget
  } .elsewhen(io.ctrl.iload) {
    PCnext := PC + 4.U
  } .otherwise {
    PCnext := PC
  }
  PC := PCnext
  val instr: UInt = imem((PCnext >> 2).asUInt())


  if(SIMULATION) {
    require(memfile.nonEmpty, "Cannot simulate with empty memory init file")
    val src = Source.fromFile(memfile)
    require(src.getLines().length <= memsize, s"Memory file (${src.getLines.length}) too large for memory size ($memsize)")
    src.close()
    loadMemoryFromFile(imem, memfile)
  } else if (memfile.nonEmpty) {
    val src = Source.fromFile(memfile)
    require(src.getLines().length <= memsize, s"Memory file (${src.getLines.length}) too large for memory size ($memsize)")
    src.close()
    loadMemoryFromFileInline(imem, memfile)
//    loadMemoryFromFile(imem, memfile)
  }

  io.id.instr := instr
  io.id.pc := PCnext
}
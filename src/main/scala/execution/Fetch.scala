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
  val imem: SyncReadMem[UInt] = SyncReadMem(memsize, UInt(INSTRUCTION_WIDTH.W))

  val notFirstCycle = RegInit(false.B)
  notFirstCycle := true.B //Should always just go to 1 after first cc
  //firstCycle is a hackish solution to bad simulation results. The reset signals in chisel testers don't work the way I want them to, causing the simulation to read the instruction at [4]
  //Almost immediately. Using firstCycle solves this. Has to start low, since registers have this value for the 1ns in chisel testers before the clock ticks

  when(io.id.branch) {
    PCnext := io.id.branchTarget
  } .elsewhen(io.ctrl.iload && notFirstCycle) {
    PCnext := PC + 4.U
  } .otherwise {
    PCnext := PC
  }
  PC := PCnext
  val instr: UInt = imem((PCnext >> 2).asUInt())


  if(SIMULATION) {
    require(memfile.nonEmpty, "Cannot simulate with empty memory init file")
    val src = Source.fromFile(memfile)
    require(src.getLines.length <= memsize, s"Memory file (${src.getLines.length}) too large for memory size ($memsize)")
    src.close()
    loadMemoryFromFile(imem, memfile)
  }
  if(INLINE) loadMemoryFromFileInline(imem, memfile) else loadMemoryFromFile(imem, memfile)

  io.id.instr := instr
  io.id.pc := PCnext
}

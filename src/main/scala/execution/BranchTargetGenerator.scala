package pipeline

import chisel3._
import chisel3.util._

class BranchTargetGenIO extends Bundle {
  val instr = Input(new BtypeInstruction)
  val pc = Input(UInt(32.W))
  val target = Output(UInt(32.W))
}

class BranchTargetGenerator extends Module {
  val io = IO(new BranchTargetGenIO)

  val sign = io.instr.targeth(6)
  val top = Mux(sign, ((1 << 14) -1).U(14.W), 0.U(14.W))
  val bot = 0.U(2.W)
  val offset = Cat(Seq(top, io.instr.targeth, io.instr.targetl, bot)).asSInt()
  val target = (io.pc.asSInt() + offset).asUInt()
  io.target := target
}

package execution

import chisel3._
import chisel3.util._

/**
 * I/O ports for [[BranchTargetGenerator]]
 */
class BranchTargetGenIO extends Bundle {
  /** The B-type instruction being evaluated */
  val instr = Input(new BtypeInstruction)
  /** The PC value from fetch stage */
  val pc = Input(UInt(32.W))
  /** The branch address to be taken if the branch evaluates true */
  val target = Output(UInt(32.W))
}

/**
 * This modules computes the branch target destination for a Btype instruction in the Decode stage. Implements [[BranchTargetGenIO]]
 */
class BranchTargetGenerator extends Module {
  val io = IO(new BranchTargetGenIO)

  val sign = io.instr.targeth(6)
  val top = Mux(sign, ((1 << 14) -1).U(14.W), 0.U(14.W))
  val bot = 0.U(2.W)
  val offset = Cat(Seq(top, io.instr.targeth, io.instr.targetl, bot)).asSInt()
  val target = (io.pc.asSInt() + offset).asUInt()
  io.target := target
}

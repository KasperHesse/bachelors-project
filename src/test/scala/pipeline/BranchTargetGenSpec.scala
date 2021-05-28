package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._


class BranchTargetGenSpec extends FlatSpec with ChiselScalatestTester with Matchers  {
  "Branch target generator" should "generate a branch target" in {
    simulationConfig()
    test(new BranchTargetGenerator) { dut =>
      val rand = scala.util.Random
      for(i <- 0 until 1) {
        val offset = -8
        val instr = BtypeInstruction(BranchComp.EQUAL, rs1=0, rs2=0, offset=offset)
        val pc = 200
        dut.io.pc.poke(pc.U)
        dut.io.instr.poke(instr)
        print(f"pc: $pc, offset: $offset%6d\t\t expect: ${pc+offset}%6d, got: ${dut.io.target.peek.litValue.toInt}%6d\n")
        dut.io.target.expect((pc+offset).U)
      }
    }
  }
}

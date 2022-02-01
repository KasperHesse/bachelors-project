package execution

import chisel3._
import chiseltest._
import utils.Config._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class BranchTargetGenSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers  {
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
        print(f"pc: $pc, offset: $offset%6d\t\t expect: ${pc+offset}%6d, got: ${dut.io.target.peek().litValue.toInt}%6d\n")
        dut.io.target.expect((pc+offset).U)
      }
    }
  }
}

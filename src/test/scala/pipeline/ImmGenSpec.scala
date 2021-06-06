package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import utils.Config._


class ImmGenSpec extends FlatSpec with ChiselScalatestTester with Matchers  {
  "ImmGen" should "sign extend an immediate" in {
    simulationConfig()
    test(new ImmediateGenerator) { dut =>
      for(i <- 0 until 20) {
        //Elaboration is finished - should be able to change FIXED_WIDTH/INT_WIDTH now
        INT_WIDTH = 3
        FRAC_WIDTH = 7
        FIXED_WIDTH = 11
        val a = genDouble() //Original double value
        val x = double2fixed(a)
        val y1 = fixed2double(x) //Rounded double value
        val immh = x.S(10,7).litValue.toInt
        val frac = x.S(6,0).litValue.toInt
        val instr = RtypeInstruction(rd=0, rs1=0, immh, frac, op=Opcode.ADD, mod=RtypeMod.VV)
        dut.io.instr.poke(instr)
        dut.clock.step()
        simulationConfig()
        val y2 = fixed2double(dut.io.imm.peek()) //Rounded double value from DUT
        print(f"$a%.6f, y1=$y1, y2=$y2\n")
        assert(y1 == y2)
      }
    }
  }
}

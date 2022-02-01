package stages

import chisel3._
import chiseltest._
import execution.{OtypeInstruction, OtypeLen, OtypeMod, OtypeSE}
import utils.Assembler
import utils.Config.{SIMULATION, simulationConfig}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class DecodeTimingSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode and timing stages"

  it should "output a vcd file" in {
    val program = "tstart clear\n" +
      "pstart single\n" +
      "estart\n" +
      "add.ss s0, s0, s0\n" +
      "eend\n" +
      "pend\n" +
      "tend\n" +
      "tstart clear\n" +
      "pstart nelemvec\n" +
      "estart\n" +
      "add.ss s1, s1, s1\n" +
      "eend\n" +
      "st.vec v0, R\n" +
      "pend\n" +
      "tend\n" +
      "tstart run\n" +
      "beq s0, s0, L1\n" +
      "pstart nelemvec\n" +
      "estart\n" +
      "add.ss s0, s0, s0\n" +
      "eend\n" +
      "pend\n" +
      "L1:\n" +
      "pstart single\n" +
      "estart\n" +
      "add.ss s2, s2, s2\n" +
      "eend\n" +
      "pend\n" +
      "tend\n"

    val memfile = "src/test/resources/meminit/decodetiming_vcd.hex"
    val instrs = Assembler.assemble(program)
    Assembler.writeMemInitFile(memfile, instrs)

    simulationConfig()
//    SIMULATION = true
    test(new DecodeTiming(clkFreq = 2000, memfile = memfile)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(200)
    }
  }
}

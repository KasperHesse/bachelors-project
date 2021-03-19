package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.ProcElemOpcode
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class RegisterFileSpec  extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Vector Register File"

  it should "VCD Dump" in {
    utils.Config.SIMULATION = true
    test(new VectorRegisterFile(width=8, depth=8, portsize=4)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.io.rs1.poke(0.U)
      dut.io.rs2.poke(1.U)
      dut.clock.step()
      dut.io.rdData1(0).expect(0.S)
      dut.io.rdData1(1).expect(1.S)
      dut.io.rdData2(0).expect(8.S)
      dut.io.rdData2(3).expect(11.S)
    }
  }


}

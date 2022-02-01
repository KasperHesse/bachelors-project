package execution

import chisel3._
import chiseltest._
import utils.Fixed._
import chisel3.experimental.BundleLiterals._
import utils.Config._
import Opcode._
import utils.Config
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ForwardingSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Forwarding unit"

  it should "work" in {
    //Pretty simple test, not thoroughly verified
    test(new Forwarding).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val d0 = (new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> RegisterFileType.VREG, _.subvec -> 0.U, _.rfUint -> 0.U)
      val d1 = (new RegisterBundle).Lit(_.reg -> 2.U, _.rf -> RegisterFileType.XREG, _.subvec -> 0.U, _.rfUint -> 0.U)
      dut.io.ex.rs1.poke(d1)
      dut.io.ex.rs2.poke(d0)
      dut.clock.step()

      dut.io.wb.rd(1).poke(d1)
      dut.io.wb.rdValids(1).poke(true.B)
      dut.io.wb.wbData(1)(2).poke(4.S)

      dut.clock.step()
      dut.io.ex.rs1newData(2).expect(4.S)
      dut.io.ex.rs1swap.expect(true.B)

      dut.clock.step()
    }
  }

}

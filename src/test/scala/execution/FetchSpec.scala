package execution

import chisel3._
import chiseltest._
import org.scalatest._
import org.scalatest.Assertions._
import utils.Fixed._

import scala.io.Source
import utils.Config._

class FetchSpec  extends FlatSpec with ChiselScalatestTester with Matchers  {

  "Fetch stage" should "load a hex memory file" in {
    val memfile = "src/test/resources/meminit/mem1.txt"
    SIMULATION = true
    test(new Fetch(memsize=8, memfile)) {dut =>
      val lines = Source.fromFile(memfile)
      dut.io.ctrl.iload.poke(true.B)
      for(line <- lines.getLines()) {
        val myint = Integer.parseInt(line, 16)
        dut.io.id.instr.expect(myint.U)
        dut.clock.step()
      }
      lines.close()
    }
  }

  "Fetch stage" should "throw an error if file is empty" in {
    SIMULATION = true
    try {
      new Fetch()
    } catch {
      case x: IllegalArgumentException => assert(true)
      case _: Throwable => assert(false)
    }
  }

  "Fetch stage" should "branch when iload is asserted" in {
    SIMULATION = false
    test(new Fetch()) {dut =>
      dut.io.id.pc.expect(0.U)
      dut.io.id.branchTarget.poke(200.U)
      dut.io.ctrl.iload.poke(true.B)
      dut.io.id.branch.poke(true.B)
      dut.clock.step()
      dut.io.id.pc.expect(200.U)
    }
  }

  "Fetch stage" should "branch when iload is not asserted" in {
    SIMULATION = false
    test(new Fetch()) {dut =>
      dut.io.id.pc.expect(0.U)
      dut.io.id.branchTarget.poke(200.U)
      dut.io.ctrl.iload.poke(false.B)
      dut.io.id.branch.poke(true.B)
      dut.clock.step()
      dut.io.id.pc.expect(200.U)
    }
  }

  "Fetch stage" should "update PC when branch is not asserted" in {
    SIMULATION = false
    test(new Fetch()) {dut =>
      dut.io.id.pc.expect(0.U)
      dut.io.id.branchTarget.poke(200.U)
      dut.io.ctrl.iload.poke(true.B)
      dut.io.id.branch.poke(false.B)
      dut.clock.step()
      dut.io.id.pc.expect(4.U)
    }
  }

  "Fetch stage" should "keep PC constant when nothing is asserted" in {
    SIMULATION = false
    test(new Fetch()) {dut =>
      dut.io.id.pc.expect(0.U)
      dut.io.id.branchTarget.poke(200.U)
      dut.io.ctrl.iload.poke(false.B)
      dut.io.id.branch.poke(false.B)
      dut.clock.step()
      dut.io.id.pc.expect(0.U)
    }
  }
}

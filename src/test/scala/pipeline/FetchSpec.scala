package pipeline

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
      var i = 0
      for(line <- lines.getLines()) {
//        dut.io.addr.poke(i.U)
        val myint = Integer.parseInt(line, 16)
        dut.io.id.instr.expect(myint.U)
        dut.clock.step()
        i += 1
      }
      lines.close()
    }
  }

  "Fetch stage" should "throw an error if file is empty" in {
    SIMULATION = true
    try {
      new Fetch()
    } catch {
      case a: IllegalArgumentException => assert(true)
      case _: Throwable => assert(false)
    }
  }
}

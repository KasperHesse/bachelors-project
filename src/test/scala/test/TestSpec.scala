package test
import chisel3._
import chiseltest._
import utils.Fixed._
import org.scalatest.{FlatSpec, Matchers}

class TestSpec extends FlatSpec with ChiselScalatestTester with Matchers  {
  behavior of "my testing class"

  it should "hopefully work" in {
    test(new Test()) {dut =>
      dut.io.i.poke(1.U)
      dut.io.j.poke(2.U)
      dut.io.k.poke(2.U)
      dut.clock.step()
      dut.io.res.expect(22.U)
    }
  }
}

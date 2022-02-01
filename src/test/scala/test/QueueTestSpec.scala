package test
import chisel3._
import chisel3.util._
import chiseltest._
import utils.Fixed._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class QueueTestSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Queues"

  def testQueue(dut: QueueTest): Unit = {
    //Poke some values, read them afterwards

    //Set up initial ready/valid pins and input data
    dut.io.valid.poke(true.B)
    dut.io.ready.poke(false.B)
    dut.io.din.poke(8.U)
    dut.io.deq_valid.expect(false.B)
    dut.clock.step()
    //Can it still take more data?
    dut.io.dout.expect(8.U)
    dut.io.deq_valid.expect(true.B)
    dut.io.enq_ready.expect(true.B)
    dut.io.qcount.expect(1.U)
    //Poke in some more values
    dut.io.din.poke(7.U)
    dut.clock.step()
    dut.io.din.poke(6.U)
    dut.clock.step()
    dut.io.din.poke(5.U)
    dut.clock.step()
    dut.io.din.poke(4.U)
    //Queue should be full now
    dut.io.enq_ready.expect(false.B)
    //Can we read?
    dut.io.ready.poke(true.B)
    dut.io.dout.expect(8.U)
    dut.clock.step()

    //Should read more values
    dut.io.dout.expect(7.U)
    dut.clock.step()
    dut.io.dout.expect(6.U)
    dut.clock.step()
    dut.io.dout.expect(5.U)
    dut.clock.step()
    dut.io.dout.expect(4.U)
    dut.clock.step()
    dut.io.dout.expect(4.U)
    dut.clock.step()
    dut.io.dout.expect(4.U)
  }

    it should "help me understand how queues work" in {
      test(new QueueTest()).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
        testQueue(c)
      }
    }
}

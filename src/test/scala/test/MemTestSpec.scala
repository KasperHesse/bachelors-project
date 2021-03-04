package test
import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class MemTestSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory test specification"

  def testSyncReadMem(dut: MemTest): Unit = {
    //Poke some values, read them afterwards

    dut.io.wrEnable.poke(true.B)
    dut.io.rdEnable.poke(true.B)
    for(i <- 0 to 20) {
      dut.io.wrAddr.poke(i.U)
      dut.io.rdAddr.poke(i.U)
      dut.io.wrData.poke(i.U)
      dut.clock.step()
      print(s"i=$i. Reading ${dut.io.rdData.peek()}\n")
    }
    dut.io.wrData.poke(21.U)
    dut.io.wrEnable.poke(false.B)
    dut.io.rdEnable.poke(true.B)

    print("READING NOW\n")
    for(i <- 0 to 20) {
      dut.io.rdAddr.poke(i.U)
      dut.clock.step()
      print(s"i=$i. Reading ${dut.io.rdData.peek()}\n")

    }
  }

//  it should "help me understand how syncreadmem works" in {
//    test(new SyncReadMemTest()).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
//      testSyncReadMem(c)
//    }
//  }
}

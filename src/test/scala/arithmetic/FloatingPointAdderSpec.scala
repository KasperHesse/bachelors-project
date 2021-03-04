package arithmetic

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{Matchers, FlatSpec}

class FloatingPointAdderSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "FPStage1"

  def FPAStage1Test(dut: FPAStage1): Unit = {
    def genAndCalc(iters: Int): Unit = {
      //Generate significand and exponents
      val r = scala.util.Random
      for(i <- 0 to iters) {
        val expa = r.nextInt(253) + 1
        val expb = r.nextInt(253) + 1
        val siga = r.nextLong() >>> 9
        val sigb = r.nextLong() >>> 9

        dut.io.sigAIn.poke(siga.U)
        dut.io.sigBIn.poke(sigb.U)
        dut.io.expa.poke(expa.U)
        dut.io.expb.poke(expb.U)
        dut.clock.step()


        val expOut = math.max(expa, expb)
        val sigaOut = if(expa > expb) siga else siga >> (expb - expa)
        val sigbOut = if(expb > expa) sigb else sigb >> (expa - expb)
        //Print inputs and expected outputs
//        print(s"Inputs: expa=${expa}, expb=${expb}, siga=$siga, sigb=$sigb \n")
//        print(s"Expected outputs: expa=$expOut, expb=$expOut, siga=${sigaOut}, sigb=${sigbOut} \n")
//        print("siga=")
//        print(dut.io.siga_out.peek())
//        print(s" sigb=${dut.io.sigb_out.peek()} \n")
        dut.io.exp.expect(expOut.U)
        dut.io.sigAOut.expect(sigaOut.U)
        dut.io.sigBOut.expect(sigbOut.U)
      }
    }
    genAndCalc(20)
  }

  def FPAStage2Test(dut: FPAStage2): Unit = {
    def genAndCalc(iters: Int): Unit = {
      //Two 56-bit values, their signs and the operand enter. One result emerges
      val r = scala.util.Random
      for (i <- 0 to iters) {
        val siga = r.nextLong() >>> 9 //Shifting by 9, as MSB must be 0, so we only have 55 usable values
        val sigb = r.nextLong() >>> 9
        val signa = r.nextBoolean()
        val signb = r.nextBoolean()
        val op = r.nextBoolean()

        dut.io.siga.poke(siga.U)
        dut.io.sigb.poke(sigb.U)
        dut.io.opIn.poke(op.B)
        dut.io.signa.poke(signa.B)
        dut.io.signb.poke(signb.B)
        dut.clock.step()

        //Expected outputs
        val a = if(signa) -1*siga else siga
        val b = if(signb) -1*sigb else sigb
        val e = if(op) a-b else a+b
                print(s"Inputs: siga=$siga, sigb=$sigb, signa=$signa, signb=$signb, op=$op \n")
                print(s"Expected output: $e \n")
                print(s"Actual output: ${dut.io.sigOut.peek()} sign ${dut.io.signOut.peek()}\n")
        dut.io.sigOut.expect(math.abs(e).U)
        dut.io.signOut.expect(if(e<0) true.B else false.B)
      }
    }
    genAndCalc(20)
  }

  it should "correctly shift and output in stage 1" in {
    test(new FPAStage1) { c =>
      FPAStage1Test(c)
    }
  }
  it should "correctly add significands in stage 2" in {
    test(new FPAStage2) { c => FPAStage2Test(c)}
  }
}

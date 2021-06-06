package pipeline

import java.io.BufferedWriter
import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._
import Opcode._
import utils.Config
import BranchComp._

import java.io._
import scala.io.Source

class IfDecSpec  extends FlatSpec with ChiselScalatestTester with Matchers {

  "Fetch/Decode" should "decode instructions and take branches" in {
    simulationConfig()
    seed("Fetch/Decode decode and branch")
    //Use assembler to generate memory init file
    val memfile = "src/test/resources/meminit/mem3.txt"
    /* Instructions
        beq s0, s1, L1 //+4
        L1: istart single
        estart
        add.vv vs2, vs1, vs0
        eend
        iend
        beq s0, s1, L1 //-20
        bne s0, s1, L2 //+24
        istart single
        estart
        sub.xx x3, x2, x0
        eend
        iend
        L2: istart single
        estart
        mul.ss s1, s2, s3
        eend
        iend
     */
    val b0 = Array(BtypeInstruction(EQUAL, 0, 1, 4)).asInstanceOf[Array[Bundle with Instruction]]
    val p1 = wrapInstructions(Array(RtypeInstruction(2, 1, 0, ADD, RtypeMod.VV)))
    val b1 = Array(BtypeInstruction(EQUAL, 0, 1, -20)).asInstanceOf[Array[Bundle with Instruction]]
    val b2 = Array(BtypeInstruction(NEQ, 0, 1, 24)).asInstanceOf[Array[Bundle with Instruction]]
    val p2 = wrapInstructions(Array(RtypeInstruction(3, 2, 0, SUB, RtypeMod.XX)))
    val p3 = wrapInstructions(Array(RtypeInstruction(1, 2, 3, MUL, RtypeMod.SS)))

    val instrs = Array.concat(b0, p1, b1, b2, p2, p3)
    val writer = new BufferedWriter(new FileWriter(memfile))
    for(instr <- instrs) {
      writer.write(("00000000" + instr.toUInt().litValue.toString(16)).takeRight(8) + "\n")
    }
    writer.close()
    test(new IfDec(memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(100)
      //How to test this? Wait for ex.newdest to be toggled, toggle through the .vv and .ss instructions?
    }
  }
}

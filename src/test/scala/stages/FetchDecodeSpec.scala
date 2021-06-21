package stages

import chisel3._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import execution._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Assembler

class FetchDecodeSpec  extends FlatSpec with ChiselScalatestTester with Matchers {

  "Fetch/Decode" should "decode instructions and take branches" in {
    simulationConfig()
    seed("Fetch/Decode decode and branch")
    //Use assembler to generate memory init file
    val memfile = "src/test/resources/meminit/fetchdecode_branches.hex"
    val program = "beq s0, s1, L1\n" + //not taken
      "L1:\n" +
      "pstart single\n" +
      "estart\n" +
      "add.vv v2, v1, v0\n" +
      "eend\n" +
      "pend\n" +
      "beq s0, s1, L1 \n" + // not taken
      "bne s0, s1, L2 \n" + //taken
      "bne s0, s0, L1 \n" + //Not taken
      "pstart single\n" +
      "estart\n" +
      "sub.xx x3, x2, x0\n" +
      "eend\n" +
      "pend\n" +
      "L2:\n" +
      "pstart single\n" +
      "estart\n" +
      "mul.ss s1, s2, s3\n" +
      "eend\n" +
      "pend\n" +
      "beq s0, s0, L1"
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
    Assembler.writeMemInitFile(memfile, Assembler.assemble(program).map(_.toLong))

//    val b0 = Array(BtypeInstruction(EQUAL, 0, 1, 4)).asInstanceOf[Array[Bundle with Instruction]]
//    val p1 = wrapInstructions(Array(RtypeInstruction(2, 1, 0, ADD, RtypeMod.VV)))
//    val b1 = Array(BtypeInstruction(EQUAL, 0, 1, -20)).asInstanceOf[Array[Bundle with Instruction]]
//    val b2 = Array(BtypeInstruction(NEQ, 0, 1, 24)).asInstanceOf[Array[Bundle with Instruction]]
//    val p2 = wrapInstructions(Array(RtypeInstruction(3, 2, 0, SUB, RtypeMod.XX)))
//    val p3 = wrapInstructions(Array(RtypeInstruction(1, 2, 3, MUL, RtypeMod.SS)))
//
//    val instrs = Array.concat(b0, p1, b1, b2, p2, p3)
//    val writer = new BufferedWriter(new FileWriter(memfile))
//    for(instr <- instrs) {
//      writer.write(("00000000" + instr.toUInt().litValue.toString(16)).takeRight(8) + "\n")
//    }
//    writer.close()
    test(new FetchDecode(memfile)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.clock.step(100)
      //How to test this? Wait for ex.newdest to be toggled, toggle through the .vv and .ss instructions?
    }
  }
}

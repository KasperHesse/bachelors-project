package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.Opcode
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._
import Opcode._

class DecodeExecuteSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "decode and execute stages"

  /**
   * Used to step through and expect the values output from the decode stage when simulating
   * @param dut the DUT
   * @param inst The instruction which generated the expected outputs
   * @param arr The array holding the register file values
   */
  def expectValues(dut: DecodeExecute, inst: RtypeInstruction, arr: Array[Array[Array[SInt]]]): Unit = {
    val vregSlotWidth = NUM_VECTOR_REGISTERS/NUM_VREG_SLOTS

    val op: Opcode.Type = inst.op
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue().toInt

    for(s <- 0 until vregSlotWidth) {
      for (i <- 0 until VECTOR_REGISTER_DEPTH by NUM_PROCELEM) {
        val op1 = arr(s + rs1 * vregSlotWidth)(0)(i)
        val op2 = arr(s + rs2 * vregSlotWidth)(0)(i)
        //This stinks, but the matching just doesn't want to work ...
        //Something with different memory references
        val ol = op.litValue
        var res = 0.S
        if(ol == ADD.litValue) {
          res =  fixedAdd(op1, op2)
        } else if (ol == SUB.litValue()) {
          res = fixedSub(op1, op2)
        } else if (ol == MUL.litValue) {
          res = fixedMul(op1, op2)
        } else if (ol == DIV.litValue) {
          res = fixedDiv(op1, op2)
        } else {
          throw new IllegalArgumentException("Unknown opcode")
        }

        //TODO: The division performed by our function vs. the circuit is pretty far off. Figure out why
//        dut.io.out.res(0).expect(res)
        assert(math.abs(fixed2double(dut.io.out.res(0).peek)-fixed2double(res)) < 1e-4)
        dut.io.out.dest.rd.expect(rd.U)
        dut.io.out.dest.subvec.expect((i/NUM_PROCELEM).U)
        dut.clock.step()
      }
    }
  }

  it should "perform elementwise VV-instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    FIXED_WIDTH = 20
    INT_WIDTH = 9
    FRAC_WIDTH = 10
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //We want to poke in some instructions, start executing and observe the outputs
      val arr = dut.decode.regFile.arr
      val vstart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.VEC)

      val subvv = RtypeInstruction(rd=2, rs1=1, rs2=2, op=SUB, mod=RtypeMod.VV)
      val mulvv = RtypeInstruction(rd=1, rs1=2, rs2=3, op=MUL, mod=RtypeMod.VV)

      val addvv = RtypeInstruction(rd=0, rs1=0, rs2=1, op=ADD, mod=RtypeMod.VV)
      val addvv2 = RtypeInstruction(rd=2, rs1=2, rs2=3, op=ADD, mod=RtypeMod.VV)
      val divvv = RtypeInstruction(rd=0, rs1=0, rs2=2, op=DIV, mod=RtypeMod.VV)
      val divvv2 = RtypeInstruction(rd=2, rs1=2, rs2=3, op=DIV, mod=RtypeMod.VV)

      //All operations to be executed
      val ops = Array(vstart, addvv, divvv2)
      val instrs = Array(addvv, divvv2)

      dut.io.idctrl.iload.poke(true.B)
      for(i <- ops.indices) {
        dut.io.in.instr.poke(ops(i).toUInt())
        dut.clock.step()
      }
      dut.io.idctrl.iload.poke(false.B)
      dut.clock.step()
      var resCnt = 0
      var i = 0
      while(i < 200 && resCnt < instrs.length) {
        if(dut.io.out.valid.peek.litToBoolean) {
          expectValues(dut, instrs(resCnt), arr)
          resCnt += 1
        } else {
          dut.clock.step()
        }
        i += 1
      }
      assert(resCnt == instrs.length)
    }
  }
}

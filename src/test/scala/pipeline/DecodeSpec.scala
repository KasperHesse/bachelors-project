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
import vector.Opcode.{ADD, DIV, MUL, SUB}

class DecodeSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode stage"

  /* TODO
      Generate simple input vectors, poke them, and see if the outputs follow suit.
      Something like
      vstart
      vadd vs0, vs0, vs1
      vmul vs1, vs2, vs3

   */

  def expectValues(dut: Decode, inst: RtypeInstruction, arr: Array[Array[Array[SInt]]]): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue().toInt
    val vregSlotWidth = NUM_VECTOR_REGISTERS/NUM_VREG_SLOTS
    for(s <- 0 until vregSlotWidth) {
      for (i <- 0 until VECTOR_REGISTER_DEPTH by NUM_PROCELEM) {
        dut.io.out.a(0).expect(arr(s + rs1 * vregSlotWidth)(0)(i))
        dut.io.out.b(0).expect(arr(s + rs2 * vregSlotWidth)(0)(i))
        dut.io.out.dest.rd.expect(rd.U)
        dut.io.out.dest.subvec.expect((i / NUM_PROCELEM).U)
        dut.clock.step()
      }
    }
  }

  def testInstructionLoad(dut: Decode): Unit = {
    val arr = dut.regFile.arr
    val vstart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val addvv = RtypeInstruction(rd=0, rs1=0, rs2=1, op=Opcode.ADD, mod=RtypeMod.VV)
    val mulvv = RtypeInstruction(rd=1, rs1=2, rs2=3, op=Opcode.MUL, mod=RtypeMod.VV)

    val ops = Array(vstart, addvv, mulvv)
    //All operations that will generate an output
    val instrs = Array(addvv, mulvv)

    dut.io.ctrl.iload.poke(true.B)
    for(i <- ops.indices) {
      dut.io.in.instr.poke(ops(i).toUInt())
      dut.clock.step()
    }
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
    //Expect values from addvv
    expectValues(dut, addvv, arr)
    //Expect values from mulvv
    expectValues(dut, mulvv, arr)
  }

  def testMultiInstructionLoad(dut: Decode): Unit = {
    val arr = dut.regFile.arr
    val vstart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val eend = OtypeInstruction(se = OtypeSE.END, iev = OtypeIEV.ELEM)
    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
    val vadd = RtypeInstruction(rd=0, rs1=0, rs2=1, op=Opcode.ADD, mod=RtypeMod.VV)
    val vmul = RtypeInstruction(rd=1, rs1=2, rs2=3, op=Opcode.MUL, mod=RtypeMod.VV)
    val vsub = RtypeInstruction(rd=2, rs1=0, rs2=3, op=Opcode.SUB, mod=RtypeMod.VV)
    val vdiv = RtypeInstruction(rd=3, rs1=1, rs2=1, op=Opcode.DIV, mod=RtypeMod.VV)

    dut.io.ctrl.iload.poke(true.B)
    dut.io.in.instr.poke(vstart.toUInt())
    dut.clock.step()
    dut.io.in.instr.poke(vadd.toUInt())
    dut.clock.step()
    dut.io.in.instr.poke(vmul.toUInt())
    dut.clock.step()
    dut.io.ctrl.iload.poke(false.B)
    dut.io.in.instr.poke(vsub.toUInt())
    dut.clock.step()
    dut.io.ctrl.iload.poke(true.B)
    //Expect values from addvv
    expectValues(dut, vadd, arr)
    //Expect values from mulvv
    expectValues(dut, vmul, arr)
    dut.io.in.instr.poke(vdiv.toUInt())
    dut.clock.step()
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
    //Expect from vsub
    expectValues(dut, vsub, arr)
    //Expect from vdiv
    expectValues(dut, vdiv, arr)
    dut.io.ctrl.state.expect(DecodeStage.sIdle)
  }



  it should "test simple value loads via waveforms" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.step(50)
    }
  }

  it should "test simple VV instruction decoding" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testInstructionLoad(dut)
    }
  }

  it should "test multiple VEC instruction loads" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testMultiInstructionLoad(dut)
    }
  }


}

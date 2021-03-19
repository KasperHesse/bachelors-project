package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.ProcElemOpcode
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._

class Instruction extends Bundle {
  val fmt = InstructionFMT
}

class DecodeSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode stage"

  /* TODO
      Generate simple input vectors, poke them, and see if the outputs follow suit.
      Something like
      vstart
      vadd vs0, vs0, vs1
      vmul vs1, vs2, vs3

   */
  def genRtype(rd: Int, rs1: Int, rs2: Int, op: UInt, mod: RtypeMod.Type): UInt = {
    (new RtypeInstruction).Lit(_.rs1 -> rs1.U, _.rs2 -> rs2.U, _.rd -> rd.U, _.op -> op, _.fmt -> InstructionFMT.RTYPE, _.mod -> mod).asUInt()
  }

  def genStype(rsrd: Int, mod: StypeMod.Type, offset: StypeOffset.Type): UInt = {
    (new StypeInstruction).Lit(_.rsrd -> rsrd.U, _.fmt -> InstructionFMT.RTYPE, _.mod -> mod, _.offset -> offset, _.nu -> 0.U).asUInt()
  }

  def genOtype(se: OtypeSE.Type, iev: OtypeIEV.Type): UInt = {
    (new OtypeInstruction).Lit(_.se -> se, _.iev -> iev, _.fmt -> InstructionFMT.OTYPE).asUInt()
  }

  def RtypeAsUInt(rd: Int, rs1: Int, rs2: Int, op: UInt, mod: RtypeMod.Type): UInt = {
    var r = 0
    r |= op.litValue().toInt
    r |= (InstructionFMT.RTYPE.litValue().toInt << 12)
    r |= (mod.litValue().toInt << 14)
    r |= (rs1 << 17)
    r |= (rs2 << 22)
    r |= (rd << 27)
    r.U(32.W)
  }

  def StypeAsUInt(rsrd: Int, mod: StypeMod.Type, offset: StypeOffset.Type): UInt = {
    var s = 0
    s |= (offset.litValue().toInt)
    s |= (InstructionFMT.STYPE.litValue().toInt << 12)
    s |= (mod.litValue().toInt << 14)
    s |= (rsrd << 17)
    s.U(32.W)
  }

  def OtypeAsUInt(se: OtypeSE.Type, iev: OtypeIEV.Type): UInt = {
    var o = 0
    o |= (se.litValue().toInt)
    o |= (iev.litValue().toInt << 1)
    o |= (InstructionFMT.OTYPE.litValue().toInt << 12)
    o.U(32.W)
  }

  /**
   * Used to step through and expect the values output from the decode stage when simulating
   * @param dut the DUT
   * @param rd Destination register of the instruction
   * @param rs1 Register select 1 of the instruction
   * @param rs2 Register select 2 of the instruction
   * @param arr The array holding the register file values
   */
  def expectValues(dut: Decode, rd: Int, rs1: Int, rs2: Int, arr: Array[Array[Array[SInt]]]): Unit = {
    val vregSlotWidth = NUM_VECTOR_REGISTERS/NUM_VREG_SLOTS
    for(s <- 0 until vregSlotWidth) {
      for (i <- 0 until VECTOR_REGISTER_DEPTH by NUM_PROCELEM) {
        dut.io.out.a(0).expect(arr(s+rs1*vregSlotWidth)(0)(i))
        dut.io.out.b(0).expect(arr(s+rs2*vregSlotWidth)(0)(i))
        dut.io.out.dest.rd.expect(rd.U)
        dut.io.out.dest.subvec.expect((i/NUM_PROCELEM).U)
        dut.clock.step()
      }
    }
  }

  def testInstructionLoad(dut: Decode): Unit = {
    val arr = dut.regFile.arr
    val vstart = OtypeAsUInt(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val addvv = RtypeAsUInt(rd=0, rs1=0, rs2=1, op=ProcElemOpcode.ADD, mod=RtypeMod.VV)
    val mulvv = RtypeAsUInt(rd=1, rs1=2, rs2=3, op=ProcElemOpcode.MUL, mod=RtypeMod.VV)

    dut.io.ctrl.iload.poke(true.B)
    dut.io.in.instr.poke(vstart)
    dut.clock.step()
    dut.io.in.instr.poke(addvv)
    dut.clock.step()
    dut.io.in.instr.poke(mulvv)
    dut.clock.step()
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
    //Expect values from addvv
    expectValues(dut, rd=0, rs1=0, rs2=1, arr)
    //Expect values from mulvv
    expectValues(dut, rd=1, rs1=2, rs2=3, arr)
  }

  def testMultiInstructionLoad(dut: Decode): Unit = {
    val arr = dut.regFile.arr
    val vstart = OtypeAsUInt(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val eend = OtypeAsUInt(se = OtypeSE.END, iev = OtypeIEV.ELEM)
    val istart = OtypeAsUInt(se = OtypeSE.START, iev = OtypeIEV.INSTR)
    val vadd = RtypeAsUInt(rd=0, rs1=0, rs2=1, op=ProcElemOpcode.ADD, mod=RtypeMod.VV)
    val vmul = RtypeAsUInt(rd=1, rs1=2, rs2=3, op=ProcElemOpcode.MUL, mod=RtypeMod.VV)
    val vsub = RtypeAsUInt(rd=2, rs1=0, rs2=3, op=ProcElemOpcode.SUB, mod=RtypeMod.VV)
    val vdiv = RtypeAsUInt(rd=3, rs1=1, rs2=1, op=ProcElemOpcode.DIV, mod=RtypeMod.VV)

    dut.io.ctrl.iload.poke(true.B)
    dut.io.in.instr.poke(eend)
    dut.clock.step()
    dut.io.in.instr.poke(vadd)
    dut.clock.step()
    dut.io.in.instr.poke(vmul)
    dut.clock.step()
    dut.io.ctrl.iload.poke(false.B)
    dut.io.in.instr.poke(vsub)
    dut.clock.step()
    dut.io.ctrl.iload.poke(true.B)
    //Expect values from addvv
    expectValues(dut, rd=0, rs1=0, rs2=1, arr)
    //Expect values from mulvv
    expectValues(dut, rd=1, rs1=2, rs2=3, arr)
    dut.io.in.instr.poke(vdiv)
    dut.clock.step()
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
    //Expect from vsub
    expectValues(dut, rd=2, rs1=0, rs2=3, arr)
    //Expect from vdiv
    expectValues(dut, rd=3, rs1=1, rs2=1, arr)
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

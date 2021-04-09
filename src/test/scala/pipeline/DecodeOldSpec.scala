package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.Opcode
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config
import utils.Config._
import vector.Opcode._

class DecodeOldSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Old decode stage"

  def expectVVvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val vReg = dut.vRegFile.arr
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    for(s <- 0 until VREG_SLOT_WIDTH) {
      for (i <- 0 until VECTOR_REGISTER_DEPTH by NUM_PROCELEM) {
        dut.io.ex.a(0).expect(vReg(s + rs1 * VREG_SLOT_WIDTH)(0)(i))
        dut.io.ex.b(0).expect(vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(i))
        dut.io.ex.dest.rd.expect((rd*VREG_SLOT_WIDTH + s).U)
        dut.io.ex.dest.subvec.expect((i / NUM_PROCELEM).U)
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
        dut.io.ex.op.expect(inst.op)
        dut.clock.step()
      }
    }
  }

  def expectXVvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val subvecsPerVreg = VECTOR_REGISTER_DEPTH/NUM_PROCELEM
    val vReg = dut.vRegFile.arr
    val xReg = dut.xRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for(i <- 0 until subvecsPerVreg) {
        for (j <- 0 until NUM_PROCELEM) {
          dut.io.ex.a(j).expect(xReg(rs1)(0)(s))
          dut.io.ex.b(j).expect(vReg(s+rs2*VREG_SLOT_WIDTH)(0)(i*NUM_PROCELEM+j))
        }
        dut.clock.step()
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
      }
    }
  }

  def expectXXvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val xReg = dut.xRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      dut.io.ex.a(i).expect(xReg(rs1)(0)(i))
      dut.io.ex.b(i).expect(xReg(rs2)(0)(i))
      dut.io.ex.dest.rd.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSVvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    val sReg = dut.sRegFile.arr
    val vReg = dut.vRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for (i <- 0 until SUBVECTORS_PER_VREG) {
        for (j <- 0 until NUM_PROCELEM) {
          val op1 = sReg(rs1) //first operand from S registers
          val op2 = vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(i * NUM_PROCELEM + j)
          dut.io.ex.a(j).expect(op1)
          dut.io.ex.b(j).expect(op2)
        }
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
        dut.io.ex.dest.rd.expect((s + rd * VREG_SLOT_WIDTH).U)
        dut.io.ex.dest.subvec.expect(i.U)
        dut.clock.step()
      }
    }
  }

  def expectSXvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val xReg = dut.xRegFile.arr
    val sReg = dut.sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = xReg(rs2)(0)(i)
      dut.io.ex.a(i).expect(op1)
      dut.io.ex.b(i).expect(op2)
      dut.io.ex.dest.rd.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSSvalues(dut: DecodeOld, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val sReg = dut.sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = sReg(rs2)
      dut.io.ex.a(i).expect(op1)
      dut.io.ex.b(i).expect(op2)
      dut.io.ex.dest.rd.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.SREG)
    }
    dut.clock.step()
  }

  def expectValues(dut: DecodeOld, instr: RtypeInstruction): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.VV.litValue) {
      expectVVvalues(dut, instr)
    } else if (mod == RtypeMod.XV.litValue()) {
      expectXVvalues(dut, instr)
    } else if (mod == RtypeMod.XX.litValue()) {
      expectXXvalues(dut, instr)
    } else if(mod == RtypeMod.SV.litValue()) {
      expectSVvalues(dut, instr)
    } else if(mod == RtypeMod.SS.litValue()) {
      expectSSvalues(dut, instr)
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }

  def genRtype(op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  def loadInstructions(ops: Array[Bundle with Instruction], dut: DecodeOld): Unit = {
    dut.io.ctrl.iload.poke(true.B)
    for(op <- ops) {
      dut.io.in.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
  }


  def genAndPoke(dut: DecodeOld, mod: RtypeMod.Type): Array[RtypeInstruction] = {
    val vstart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val add = genRtype(ADD, mod)
    val sub = genRtype(SUB, mod)
    val mul = genRtype(MUL, mod)
    val div = genRtype(DIV, mod)

    val ops = Array(vstart, add, div, mul, sub)
    val instrs = Array(add, div, mul, sub)
    loadInstructions(ops, dut)

    instrs
  }


  def testDecode(dut: DecodeOld, mod: RtypeMod.Type): Unit = {
    val instrs = genAndPoke(dut, mod)
    for(inst <- instrs) {
      expectValues(dut, inst)
    }
  }

  def testMVPInstruction(dut: DecodeOld): Unit = {
    val seed = scala.util.Random.nextLong()
    print(s"MVP decode. Using seed ${seed}\n")
    scala.util.Random.setSeed(seed)

    val ke = dut.KE.KE
    val vstart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.VEC)
    val mvp = genRtype(op=MAC, mod=RtypeMod.MVP)

    val ops = Array(vstart, mvp)
    val instrs = Array(mvp)

    loadInstructions(ops, dut)
    for(instr <- instrs) {
      val arr = dut.vRegFile.arr
      val rs1 = instr.rs1.litValue.toInt
      val rd = instr.rd.litValue.toInt
      for(s <- 0 until VREG_SLOT_WIDTH) {
        for (y <- 0 until KE_SIZE / NUM_PROCELEM) {
          for (x <- 0 until KE_SIZE / NUM_PROCELEM) {
            for (c <- 0 until NUM_PROCELEM) {
              for (i <- 0 until NUM_PROCELEM) {
                dut.io.ex.a(i).expect(double2fixed(ke(y * KE_SIZE + x * NUM_PROCELEM + c)(i)).S)
                dut.io.ex.b(i).expect(arr(rs1 * VREG_SLOT_WIDTH + s)(0)(x * NUM_PROCELEM + c))
                dut.io.ex.dest.subvec.expect(y.U)
                dut.io.ex.dest.rd.expect((rd * VREG_SLOT_WIDTH + s).U)
                dut.io.ex.macLimit.expect(KE_SIZE.U)
              }
              dut.clock.step()
              dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
            }
          }
        }
      }
    }
  }

  it should "test VV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    test(new DecodeOld).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testDecode(dut, RtypeMod.VV)
    }
  }

  it should "test XV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    test(new DecodeOld).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testDecode(dut, RtypeMod.XV)
    }
  }

  it should "test XX instruction load and decode" in {
    SIMULATION = true
    test(new DecodeOld) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"XX decode. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testDecode(dut, RtypeMod.XX)
    }
  }

  it should "test SV instruction load and decode" in {
    SIMULATION = true
    test(new DecodeOld) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"SV decode. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testDecode(dut, RtypeMod.SV)
    }
  }

  it should "test SS instruction load and decode" in {
    SIMULATION = true
    test(new DecodeOld) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"SS decode. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testDecode(dut, RtypeMod.SS)
    }
  }

  it should "test MVP instruction load and decode" in {
    SIMULATION = true
    KE_SIZE = 12
    NUM_PROCELEM = 4
    VECTOR_REGISTER_DEPTH = 12
    NUM_VECTOR_REGISTERS = 8
    NUM_VREG_SLOTS = 2
    FIXED_WIDTH = 16
    INT_WIDTH = 15
    FRAC_WIDTH = 0
    Config.checkRequirements()

    //With above config, we have 4 registers per slot.
    //Starting at rd=0, subvec=0, it should move through subvec=0,1,2
    //col should be in range [0;3], X and Y should be in range [0;2], slotSelect should be in range [0;3]
    test(new DecodeOld).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testMVPInstruction(dut)
    }
  }



}

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

class DecodeSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode stage"

  //This test. Load in a simple set of VV instructions, test whether outputs are asserted correctly
  //Can reuse much of the original decode tester

  def expectVVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val vReg = dut.threads(0).vRegFile.arr
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

  def expectXVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val subvecsPerVreg = VECTOR_REGISTER_DEPTH/NUM_PROCELEM
    val vReg = dut.threads(0).vRegFile.arr
    val xReg = dut.threads(0).xRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for(i <- 0 until subvecsPerVreg) {
        for (j <- 0 until NUM_PROCELEM) {
          dut.io.ex.a(j).expect(xReg(rs1)(0)(s))
          dut.io.ex.b(j).expect(vReg(s+rs2*VREG_SLOT_WIDTH)(0)(i*NUM_PROCELEM+j))
        }
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
        dut.clock.step()

      }
    }
  }

  def expectXXvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val xReg = dut.threads(0).xRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      dut.io.ex.a(i).expect(xReg(rs1)(0)(i))
      dut.io.ex.b(i).expect(xReg(rs2)(0)(i))
      dut.io.ex.dest.rd.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    val sReg = dut.threads(0).sRegFile.arr
    val vReg = dut.threads(0).vRegFile.arr

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

  def expectSXvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val xReg = dut.threads(0).xRegFile.arr
    val sReg = dut.threads(0).sRegFile.arr

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

  def expectSSvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val sReg = dut.threads(0).sRegFile.arr

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

  def expectValues(dut: Decode, instr: RtypeInstruction): Unit = {
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
    } else if (mod == RtypeMod.SX.litValue()) {
      expectSXvalues(dut, instr)
    } else {
      expectSSvalues(dut, instr)
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }

  def loadInstructions(ops: Array[Bundle with Instruction], dut: Decode): Unit = {
    dut.io.ctrl.iload.poke(true.B)
    for(op <- ops) {
      dut.io.in.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
  }

  /**
   * Generates an Rtype instruction with a specified opcode and modifier and random rd, rs1 and rs2 fields
   * @param op The Opcode to use
   * @param mod The modifier to use
   * @return The generated instruction
   */
  def genRtype(op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  /**
   * Generates a random Rtype-instruction (random mod+opcode)
   * @return
   */
  def genRtype(): RtypeInstruction = {
    import RtypeMod._
    val rand = scala.util.Random

    val mods = Array(VV, XV, SV, XX, SX, SS)
    val opcodes = Array(ADD, SUB, MUL, DIV)
    val mod = mods(rand.nextInt(mods.length))
    val op = opcodes(rand.nextInt(opcodes.length))
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  /**
   * Generates and pokes in a number of instructions with the same Rtype-modifier
   * @param dut the DUT
   * @param mod The Rtype-modifier to use for all of the instructions
   * @return An array of the executable instructions
   */
  def genAndPoke(dut: Decode, mod: RtypeMod.Type): Array[RtypeInstruction] = {
    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)
    val add = genRtype(ADD, mod)
    val sub = genRtype(SUB, mod)
    val mul = genRtype(MUL, mod)
    val div = genRtype(DIV, mod)

    val ops = Array(istart, estart, add, div, mul, sub, eend, iend)
    val instrs = Array(add, div, mul, sub)
    loadInstructions(ops, dut)

    instrs
  }

  /**
   * Generates and pokes a number of instructions with random Rtype modifiers
   * @param dut The DUT
   * @return
   */
  def genAndPoke(dut: Decode): Array[RtypeInstruction] = {
    genAndPoke(dut, 1)
//    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
//    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
//    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
//    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)
//
//    //Generate random instrs.
//    val instrs = Array.fill(4)(genRandomRtype())
//    val ops = Array(istart, estart, instrs(0), instrs(1), instrs(2), instrs(3), eend, iend)
//    val a1 = Array(istart, estart).asInstanceOf[Array[Bundle with Instruction]]
//    val a2 = Array(eend, iend).asInstanceOf[Array[Bundle with Instruction]]
//    val opss = Array.concat(a1, instrs.asInstanceOf[Array[Bundle with Instruction]], a2)
//    loadInstructions(opss, dut)
//
//    instrs
  }

  /**
   * Generates and pokes a number of instructions with random Rtype modifiers
   * @param dut The DUT
   * @param len The length of the instruction (Single, Ndof or Nelem operations)
   * @return
   */
  def genAndPoke(dut: Decode, len: Int): Array[RtypeInstruction] = {
    var length = OtypeLen.SINGLE
    if(len == NDOF) {
      length = OtypeLen.NDOF
    } else if (len == NELEM) {
      length = OtypeLen.NELEM
    } else if (len != 1) {
      throw new IllegalArgumentException("length must be 1, NELEM or NDOF")
    }

    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR, length)
    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)

    //Generate random instrs.
    val instrs = Array.fill(4)(genRtype())
    val a1 = Array(istart, estart).asInstanceOf[Array[Bundle with Instruction]]
    val a2 = Array(eend, iend).asInstanceOf[Array[Bundle with Instruction]]
    val ops = Array.concat(a1, instrs.asInstanceOf[Array[Bundle with Instruction]], a2)
    loadInstructions(ops, dut)

    instrs
  }

  def testDecode(dut: Decode, mod: RtypeMod.Type): Unit = {
    val instrs = genAndPoke(dut, mod)
    //Step until outputs are available
    while(dut.io.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue()) {
      dut.clock.step()
    }
    for(inst <- instrs) {
      expectValues(dut, inst)
    }
    //TODO: Modify the above to not expect values when length > single
    dut.clock.step(5)
  }

  /**
   * Computes and prints the random seed to be used for this tester. Returns the ID of the thread object instantiated
   * @param name The name of the test
   * @return ID to be used for Thread object
   */
  def seed(name: String): Unit = {
    val seed = scala.util.Random.nextLong()
    scala.util.Random.setSeed(seed)
    print(s"$name. Using seed $seed\n")
  }
  it should "test VV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("VV Decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testDecode(dut, RtypeMod.VV)
    }
  }

  it should "test XV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("XV Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.XV)
    }
  }

  it should "test SV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("SV Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SV)
    }
  }

  it should "test XX instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("XX Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.XX)
    }
  }
  it should "test SX instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("SX Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SX)
    }
  }

  it should "test SS instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("SS Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SS)
    }
  }

  //Generate random instructions of different kinds
  it should "Decode a random instruction mix" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("Random mix decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val instrs = genAndPoke(dut)
      //Step until outputs are available
      while(dut.io.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue()) {
        dut.clock.step()
      }
      for(inst <- instrs) {
        expectValues(dut, inst)
      }
      dut.clock.step(5)
    }
  }

  it should "Decode a random mix for multiple iterations" in {
    SIMULATION = true
    NDOF = 3*NUM_PROCELEM
    Config.checkRequirements()
    seed("Random mix decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val iters = NDOF/NUM_PROCELEM
      val instrs = genAndPoke(dut, NDOF)
      var i = 0
      while(i < iters) {
        //Step until outputs are available
        while ((dut.io.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue())
        .&& (dut.io.threadCtrl(1).stateUint.peek.litValue != ThreadState.sExec.litValue) ) {
          dut.clock.step()
        }
        for (inst <- instrs) {
          expectValues(dut, inst)
        }
        i += 1
        dut.clock.step()
      }
      dut.clock.step(5)
    }
  }
}

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
      for (i <- 0 until VREG_DEPTH by NUM_PROCELEM) {
        dut.io.ex.a(0).expect(vReg(s + rs1 * VREG_SLOT_WIDTH)(0)(i))
        dut.io.ex.b(0).expect(vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(i))
        dut.io.ex.dest.reg.expect((rd*VREG_SLOT_WIDTH + s).U)
        dut.io.ex.dest.subvec.expect((i / NUM_PROCELEM).U)
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
        dut.io.ex.op.expect(inst.op)
        dut.clock.step()
      }
    }
  }

  def expectKVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val smpr = KE_SIZE / NUM_PROCELEM //Submatrices per row
    val KE = dut.threads(0).KE.KE
    val vReg = dut.threads(0).vRegFile.arr
    val rs1 = inst.rs1.litValue.toInt
    val rd = inst.rd.litValue().toInt

    //smpr rows, KE_SIZE results on each row
    //results on each row are split into groups of NUM_PROCELEM
    for(w <- 0 until VREG_SLOT_WIDTH) {
      for (r <- 0 until smpr) {
        val slices = KE.slice(r * KE_SIZE, (r + 1) * KE_SIZE)
        for (s <- 0 until KE_SIZE) {
          for (i <- 0 until NUM_PROCELEM) {
            val a = vReg(rs1 * VREG_SLOT_WIDTH + w)(0)(s)
            val b = double2fixed(slices(s)(i)).S
            dut.io.ex.a(i).expect(a)
            dut.io.ex.b(i).expect(b)
          }
          dut.clock.step()
        }
      }
    }
  }

  def expectXVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val subvecsPerVreg = VREG_DEPTH/NUM_PROCELEM
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
      dut.io.ex.dest.reg.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSVvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    val sReg = dut.sRegFile.arr
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
        dut.io.ex.dest.reg.expect((s + rd * VREG_SLOT_WIDTH).U)
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
    val sReg = dut.sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = xReg(rs2)(0)(i)
      dut.io.ex.a(i).expect(op1)
      dut.io.ex.b(i).expect(op2)
      dut.io.ex.dest.reg.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSSvalues(dut: Decode, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val sReg = dut.sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = sReg(rs2)
      dut.io.ex.a(i).expect(op1)
      dut.io.ex.b(i).expect(op2)
      dut.io.ex.dest.reg.expect(rd)
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
    } else if(mod == RtypeMod.SX.litValue()) {
      expectSXvalues(dut, instr)
    } else if (mod == RtypeMod.SS.litValue()) {
      expectSSvalues(dut, instr)
    } else if (mod == RtypeMod.KV.litValue()) {
      expectKVvalues(dut, instr)
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }


  def loadInstructions(ops: Array[Bundle with Instruction], dut: Decode): Unit = {
    dut.io.ctrl.iload.poke(true.B)
    for(op <- ops) {
      dut.io.fe.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()
  }

  /**
   * Generates and pokes in a number of instructions with the same Rtype-modifier
   * @param dut the DUT
   * @param mod The Rtype-modifier to use for all of the instructions
   * @return An array of the executable instructions
   */
  def genAndPokeRtype(dut: Decode, mod: RtypeMod.Type): Array[RtypeInstruction] = {
    val add = genRtype(ADD, mod)
    val sub = genRtype(SUB, mod)
    val mul = genRtype(MUL, mod)
    val div = genRtype(DIV, mod)

    val instrs = Array(add, div, mul, sub)
    val ops = wrapInstructions(instrs)
    loadInstructions(ops, dut)

    instrs
  }

  /**
   * Generates and pokes a number of instructions with random Rtype modifiers
   * @param dut The DUT
   * @return
   */
  def genAndPoke(dut: Decode): Array[RtypeInstruction] = {
    genAndPoke(dut, OtypeLen.SINGLE)
  }

  /**
   * Generates and pokes a number of instructions with random Rtype modifiers for multiple iterations
   * @param dut The DUT
   * @param len The length of the instruction (Single, Ndof or Nelem operations)
   * @return
   */
  def genAndPoke(dut: Decode, len: OtypeLen.Type): Array[RtypeInstruction] = {
    val instrs = Array.fill(4)(genRtype())
    val ops = wrapInstructions(instrs, len)
    loadInstructions(ops, dut)

    instrs
  }


  def testDecode(dut: Decode, mod: RtypeMod.Type): Unit = {
    val instrs = genAndPokeRtype(dut, mod)
    //Step until outputs are available
    while(dut.io.ctrl.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue()) {
      dut.clock.step()
    }
    for(inst <- instrs) {
      expectValues(dut, inst)
    }
    dut.clock.step(5)
  }


  it should "test KV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("KV decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val instrs = Array(genRtype(MAC, RtypeMod.KV))
      val ops = wrapInstructions(instrs)
      loadInstructions(ops, dut)
      //Step until outputs are available
      while(dut.io.ctrl.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue()) {
        dut.clock.step()
      }
      for(inst <- instrs) {
        expectValues(dut, inst)
      }
      dut.clock.step(5)
    }
  }

  it should "test VV instruction load and decode" in {
    genericConfig()
    seed("VV Decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      testDecode(dut, RtypeMod.VV)
    }
  }

  it should "test XV instruction load and decode" in {
    genericConfig()
    seed("XV Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.XV)
    }
  }

  it should "test SV instruction load and decode" in {
    genericConfig()
    seed("SV Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SV)
    }
  }

  it should "test XX instruction load and decode" in {
    genericConfig()
    seed("XX Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.XX)
    }
  }
  it should "test SX instruction load and decode" in {
    genericConfig()
    seed("SX Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SX)
    }
  }

  it should "test SS instruction load and decode" in {
    genericConfig()
    seed("SS Decode")
    test(new Decode) { dut =>
      testDecode(dut, RtypeMod.SS)
    }
  }


  //Generate random instructions of different kinds
  it should "Decode a random instruction mix" in {
    genericConfig()
    seed("Random mix decode")
    test(new Decode).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val instrs = genAndPoke(dut)
      //Step until outputs are available
      while(dut.io.ctrl.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue()) {
        dut.clock.step()
      }
      for(inst <- instrs) {
        expectValues(dut, inst)
      }
      dut.clock.step(5)
    }
  }

  "Decode stage" should "Decode a random mix for multiple iterations" in {
    genericConfig()
    seed("Random mix decode")
    test(new Decode) { dut =>
      val iters = (if(NDOF % ELEMS_PER_VSLOT == 0) NDOF else ((NDOF/ELEMS_PER_VSLOT)+1)*ELEMS_PER_VSLOT)/ELEMS_PER_VSLOT
      val instrs = genAndPoke(dut, OtypeLen.NDOF)
      var i = 0
      while(i < iters) {
        //Step until outputs are available
        while ((dut.io.ctrl.threadCtrl(0).stateUint.peek.litValue() != ThreadState.sExec.litValue())
        .&& (dut.io.ctrl.threadCtrl(1).stateUint.peek.litValue != ThreadState.sExec.litValue) ) {
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

  /**
   * Tests branching behavior of the decode stage
   * @param dut The decode stage
   * @param comp The comparison to perform
   * @param rs1 Register source 1
   * @param rs2 Register source 2
   * @param offset The branch offset (+/- 200)
   */
  def testBranchBehavior(dut: Decode, comp: BranchComp.Type, rs1: Int, rs2: Int, offset: Int): Unit = {
    import BranchComp._
    require(math.abs(offset) <= 200)

    val sReg = dut.sRegFile.arr
    val Binst = BtypeInstruction(comp, rs1, rs2, offset)
    dut.io.fe.instr.poke(Binst.toUInt())
    dut.io.fe.pc.poke(200.U)
    dut.io.ctrl.iload.poke(false.B)
    dut.clock.step()

    val taken: Boolean = if(comp.litValue == EQUAL.litValue) {
      sReg(rs1).litValue == sReg(rs2).litValue
    } else if (comp.litValue == NEQ.litValue) {
      sReg(rs1).litValue() != sReg(rs2).litValue()
    } else if (comp.litValue == LT.litValue) {
      sReg(rs1).litValue() < sReg(rs2).litValue()
    } else if (comp.litValue == GEQ.litValue) {
      sReg(rs1).litValue() >= sReg(rs2).litValue
    } else {
      throw new IllegalArgumentException("Unable to decode comp value")
    }
    dut.io.fe.branchTarget.expect((200+offset).U)
    dut.io.fe.branch.expect(taken.B)
  }

  "Decode stage" should "take a branch when EQUAL" in {
    genericConfig()
    seed("Decode branch equal")
    test(new Decode) {dut =>
      testBranchBehavior(dut, BranchComp.EQUAL, 0, 0, -8)
    }
  }

  "Decode stage" should "take a branch when NOT EQUAL" in {
    genericConfig()
    seed("Decode branch equal")
    test(new Decode) {dut =>
      testBranchBehavior(dut, BranchComp.NEQ, 1, 0, -8)
    }
  }

  //This simply attempts a bunch of branches in a row, to test if they work correctly
  "Decode stage" should "test branch behavior" in {
    genericConfig()
    seed("Decode branch multiple")
    test(new Decode) {dut =>
      val rand = scala.util.Random
      val sReg = dut.sRegFile.arr
      var pc = 200
      for(i <- 0 until 20) {
        //Generate random rs1, rs2, comp values. Use to test and update
        val rs1 = rand.nextInt(NUM_SREG)
        val rs2 = rand.nextInt(NUM_SREG)
        val compInt = rand.nextInt(4)
        val comp = compInt match {
          case 0 => BranchComp.EQUAL
          case 1 => BranchComp.NEQ
          case 2 => BranchComp.LT
          case 3 => BranchComp.GEQ
          case _ => throw new IllegalArgumentException("Unable to generate comp")
        }
        val taken = compInt match {
          case 0 => sReg(rs1).litValue == sReg(rs2).litValue
          case 1 => sReg(rs1).litValue != sReg(rs2).litValue
          case 2 => sReg(rs1).litValue <  sReg(rs2).litValue()
          case 3 => sReg(rs1).litValue >= sReg(rs2).litValue()
        }
        val offset = rand.nextInt(8) * 4 * {if(rand.nextBoolean()) 1 else -1}

        val Binstr = BtypeInstruction(comp, rs1, rs2, offset)
        dut.io.fe.instr.poke(Binstr.toUInt())
        dut.io.fe.pc.poke(pc.U)

        dut.clock.step()

        dut.io.fe.branchTarget.expect((pc + offset).U)
        dut.io.fe.branch.expect(taken.B)
        if(taken) {
          pc += offset
        }
      } //end for
    } //end test()
  } //end suite
} //end class

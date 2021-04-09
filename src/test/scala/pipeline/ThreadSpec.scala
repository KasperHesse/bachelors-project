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
import pipeline.ThreadState._

class ThreadSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decoder thread"

  def expectVVvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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

  def expectXVvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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

  def expectXXvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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

  def expectSVvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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


  def expectSXvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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

  def expectSSvalues(dut: Thread, inst: RtypeInstruction): Unit = {
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

  def expectValues(dut: Thread, instr: UInt): Unit = {
    val i = RtypeInstruction(instr)
    val mod = i.mod.litValue
    if(mod == RtypeMod.VV.litValue) {
      expectVVvalues(dut, i)
    } else if (mod == RtypeMod.XV.litValue()) {
      expectXVvalues(dut, i)
    } else if (mod == RtypeMod.XX.litValue()) {
      expectXXvalues(dut, i)
    } else if(mod == RtypeMod.SV.litValue()) {
      expectSVvalues(dut, i)
    } else if(mod == RtypeMod.SS.litValue()) {
      expectSSvalues(dut, i)
    } else if(mod == RtypeMod.SX.litValue()) {
      expectSXvalues(dut, i)
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

  def genInstructions(dut: Thread, mod: RtypeMod.Type): Array[Bundle with Instruction] = {
    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)
    val add = genRtype(ADD, mod)
    val sub = genRtype(SUB, mod)
    val mul = genRtype(MUL, mod)
    val div = genRtype(DIV, mod)

    val ops = Array(istart, estart, add, div, mul, sub, eend, iend)
    ops
  }

  /**
   * Tests the thread module for correctly decoding instruction packets that only contain execution instructions
   * @param dut The Thread DUT
   * @param mod The Rtype-modifier to be used when generating instructions.
   * @param numRes Number of results which should be observed before finishing the test
   * @note Should only be used for instruction packets of the kind (istart, estart, {instructions}, eend, iend)
   */
  def testThread(dut: Thread, mod: RtypeMod.Type, numRes: Int = 4): Unit = {
    val instrs = genInstructions(dut, mod)

    //Poke default values
    dut.io.i.poke(0.U)
    dut.io.j.poke(0.U)
    dut.io.k.poke(0.U)
    dut.io.fin.poke(false.B)
    dut.io.progress.poke(0.U)
    dut.io.stateIn.poke(sEstart)
    dut.io.instr.poke(0.U)
    dut.io.start.poke(true.B)

    var fin: Boolean = false
    var i: Int = 0
    var resCnt: Int = 0

    while(!fin && i < 200) {
      val ip = dut.io.ip.peek.litValue.toInt
      dut.io.instr.poke(instrs(ip).toUInt())

      //Assign stateIn to speed through load and store stages
      if(dut.io.stateOutUint.peek.litValue == sLoad.litValue) {
        dut.io.stateIn.poke(sEend)
        dut.io.start.poke(false.B)
      } else if (dut.io.stateOutUint.peek.litValue == sExec.litValue) {
        dut.io.stateIn.poke(sEstart)
      }

      //Assert 'fin' once end state has been reached
      if(dut.io.stateOutUint.peek.litValue == sIend.litValue && resCnt >= numRes) {
        fin = true
      }
      dut.io.fin.poke(fin.B)

      //Expect output values. Start doing this on the first clock cycle of each instruction
      //Currently pretty ugly, but that's what we have to work with, I guess
      //We need the final check since IP is kept constant for one cycle before moving to sLoad, and we should not expect in that cycle
      if(dut.io.stateOutUint.peek.litValue == sExec.litValue && dut.io.ctrl.firstCycle.peek.litToBoolean && dut.io.ex.newDest.peek.litToBoolean /*&& resCnt < numRes */) {
        expectValues(dut, instrs(ip).toUInt())
        resCnt += 1
      } else {
        dut.clock.step()
      }

      i += 1
    }
    dut.clock.step(3)
    assert(resCnt == numRes)

  }

  /**
   * Computes and prints the random seed to be used for this tester. Returns the ID of the thread object instantiated
   * @param name The name of the test
   * @return ID to be used for Thread object
   */
  def seed(name: String): Int = {
    val seed = scala.util.Random.nextLong()
    scala.util.Random.setSeed(seed)
    val id = scala.util.Random.nextInt(2) //1 or 2
    print(s"$name. Using seed $seed. Thread id $id\n")
    id
  }

  it should "test VV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("VV thread decode")
    test(new Thread(id)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testThread(dut, RtypeMod.VV)
    }
  }

  it should "test XV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("XV thread decode")
    test(new Thread(id)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testThread(dut, RtypeMod.XV)
    }
  }

  it should "test XX instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("XX thread decode")
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.XX)
    }
  }

  it should "test SV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("SV thread decode")
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.SV)
    }
  }

  it should "test SX instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("SX thread decode")
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.SX)
    }
  }

  it should "test SS instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("SS thread decode")
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.SS)
    }
  }

  //This test uses the same instruction mix as previously, but keeps 'fin' low until multiple iterations have been performed.
  it should "perform multiple runs if length is NDOF or NELEM" in {
    SIMULATION = true
    Config.checkRequirements()
    val id = seed("VV length NDOF/NELEM")
    test(new Thread(id)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testThread(dut, RtypeMod.VV, 16)
    }
  }

  it should "go back to idle when id=2 and fin=true" in {
    SIMULATION = true
    Config.checkRequirements()
    test(new Thread(1)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)

      dut.io.start.poke(true.B)
      dut.io.fin.poke(true.B)
      dut.io.instr.poke(istart.toUInt())
      dut.clock.step()

      assert(dut.io.stateOutUint.peek.litValue == sWait1.litValue)
      dut.io.stateIn.poke(sEstart)
      dut.clock.step()

      assert(dut.io.stateOutUint.peek().litValue() == sWait2.litValue)
      dut.clock.step(5)
      assert(dut.io.stateOutUint.peek().litValue() == sWait2.litValue)
      dut.io.stateIn.poke(sEend)
      dut.clock.step()

      assert(dut.io.stateOutUint.peek.litValue() == sIdle.litValue())
    }
  }
}

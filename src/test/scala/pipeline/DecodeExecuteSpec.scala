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
import utils.Config

class DecodeExecuteSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "decode and execute stages"

  /**
   * Used to step through and expect the values output from the decode-execute stage when simulating
   * @param dut the DUT
   * @param inst The instruction which generated the expected outputs
   */
  def expectVVvalues(dut: DecodeExecute, inst: RtypeInstruction): Unit = {
    val vReg = dut.decode.threads(0).vRegFile.arr
    val subvectorsPerRegister = VECTOR_REGISTER_DEPTH/NUM_PROCELEM

    val op: Opcode.Type = inst.op
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue().toInt


    //Special case that we need to handle
    if(op.litValue == MAC.litValue) {
      //Sum and multiply over all values
      //Result generated at each multiplication stage
      val tempResults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
      for(s <- 0 until VREG_SLOT_WIDTH) {
        for(u <- 0 until subvectorsPerRegister) {
          for(k <- 0 until NUM_PROCELEM) {
            val sv1 = vReg(rs1*VREG_SLOT_WIDTH+s)(0)(u*NUM_PROCELEM+k)
            val sv2 = vReg(rs2*VREG_SLOT_WIDTH+s)(0)(u*NUM_PROCELEM+k)
            tempResults(k) = fixedAdd(tempResults(k), fixedMul(sv1, sv2))
          }
        }
      }

      for(i <- 0 until NUM_PROCELEM) {
        dut.io.out.res(i).expect(tempResults(i))
      }
      dut.clock.step()
    } else {
      for (s <- 0 until VREG_SLOT_WIDTH) {
        for (i <- 0 until VECTOR_REGISTER_DEPTH by NUM_PROCELEM) {
          //Extract operands used (a[0] and b[0] in each iteration
          val op1 = vReg(s + rs1 * VREG_SLOT_WIDTH)(0)(i)
          val op2 = vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(i)
          val res = calculateRes(op, op1, op2)
          assert(math.abs(fixed2double((dut.io.out.res(0).peek.litValue - res.litValue).toLong)) < 1e-2)
          dut.io.out.dest.rd.expect((rd*VREG_SLOT_WIDTH+s).U)
          dut.io.out.dest.subvec.expect((i / NUM_PROCELEM).U)
          dut.clock.step()
        }
      }
    }
  }

  def expectXVvalues(dut: DecodeExecute, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue().toInt
    val op = inst.op
    val subvecsPerVreg = VECTOR_REGISTER_DEPTH/NUM_PROCELEM
    val vReg = dut.decode.threads(0).vRegFile.arr
    val xReg = dut.decode.threads(0).xRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for(i <- 0 until subvecsPerVreg) {
        for (j <- 0 until NUM_PROCELEM) {
          val op1 = xReg(rs1)(0)(s)
          val op2 = vReg(s+rs2*VREG_SLOT_WIDTH)(0)(i*NUM_PROCELEM+j)
          val res = calculateRes(op, op1, op2)
          assert(math.abs(fixed2double(dut.io.out.res(j).peek) - fixed2double(res)) < 1e-2)
          dut.io.out.dest.rd.expect((rd*VREG_SLOT_WIDTH + s).U)
          dut.io.out.dest.subvec.expect(i.U)
        }
        dut.io.out.dest.rf.expect(RegisterFileType.VREG)
        dut.clock.step()
      }
    }
  }

  def expectXXvalues(dut: DecodeExecute, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val op = inst.op
    val xReg = dut.decode.threads(0).xRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = xReg(rs1)(0)(i)
      val op2 = xReg(rs2)(0)(i)
      val res = calculateRes(op, op1, op2)
      assert(math.abs(fixed2double((dut.io.out.res(i).peek.litValue - res.litValue).toLong)) < 1e-2)
      dut.io.out.dest.rd.expect(rd)
      dut.io.out.dest.subvec.expect(0.U)
      dut.io.out.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSVvalues(dut: DecodeExecute, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    val op = inst.op
    val sReg = dut.decode.threads(0).sRegFile.arr
    val vReg = dut.decode.threads(0).vRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for (i <- 0 until SUBVECTORS_PER_VREG) {
        for (j <- 0 until NUM_PROCELEM) {
          val op1 = sReg(rs1) //first operand from S registers
          val op2 = vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(i * NUM_PROCELEM + j)
          val res = calculateRes(op, op1, op2)
          assert(math.abs(fixed2double((dut.io.out.res(j).peek.litValue - res.litValue).toLong)) < 1e-2)
          dut.io.out.dest.rf.expect(RegisterFileType.VREG)
          dut.io.out.dest.rd.expect((s + rd * VREG_SLOT_WIDTH).U)
          dut.io.out.dest.subvec.expect(i.U)
        }
        dut.clock.step()
      }
    }
  }


  def expectSSvalues(dut: DecodeExecute, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val op = inst.op
    val sReg = dut.decode.threads(0).sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = sReg(rs2)
      val res = calculateRes(op, op1, op2)
      assert(math.abs(fixed2double((dut.io.out.res(i).peek.litValue - res.litValue).toLong)) < 1e-2)
      dut.io.out.dest.rd.expect(rd)
      dut.io.out.dest.subvec.expect(0.U)
      dut.io.out.dest.rf.expect(RegisterFileType.SREG)
    }
    dut.clock.step()
  }

  /**
   * Method that calls the correct version of expectZZvalues, where ZZ is some combination of X, V and S
   *
   * @param dut The DUT
   * @param instr The instruction to expect results from
   */
  def expectValues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.VV.litValue) {
      expectVVvalues(dut, instr)
    } else if (mod == RtypeMod.XV.litValue()) {
      expectXVvalues(dut, instr)
    } else if (mod == RtypeMod.XX.litValue()) {
      expectXXvalues(dut, instr)
    } else if (mod == RtypeMod.SV.litValue()) {
      expectSVvalues(dut, instr)
    } else if (mod == RtypeMod.SS.litValue()) {
      expectSSvalues(dut, instr)
    } else {
        throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }


  /**
   * Tests an instruction mix where all instructions have the same Rtype-modifier
   * @param dut The DUT
   */
  def testSameMod(dut: DecodeExecute, mod: RtypeMod.Type): Unit = {
    val instrs = genAndPoke(dut, mod)

    var resCnt = 0
    var i = 0
    while(i < 200 && resCnt < instrs.length) {
      if(dut.io.out.valid.peek.litToBoolean) {
        expectValues(dut, instrs(resCnt))
        resCnt += 1
      } else {
        dut.clock.step()
      }
      i += 1
    }
    assert(resCnt == instrs.length)
    dut.io.exctrl.count.expect(0.U)
  }


  def testMVP(dut: DecodeExecute): Unit = {
    val smpr = KE_SIZE / NUM_PROCELEM //Sub-matrices per row
    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)
    val mvp = RtypeInstruction(0, 0, 1, op = Opcode.MAC, mod = RtypeMod.MVP)

    val ops = Array(istart, estart, mvp, eend, iend)
    val instrs = Array(mvp)
    loadInstructions(ops, dut)

    //Keep polling and wait for the results.
    var i = 0
    var resCnt = 0
    val KE = dut.decode.threads(0).KE.KE
    val arr = dut.decode.threads(0).vRegFile.arr
    val Rd = mvp.rd.litValue.toInt
    val rs1 = mvp.rs1.litValue.toInt

    val resMax = KE_SIZE * VREG_SLOT_WIDTH / NUM_PROCELEM
    while (i < 400 && resCnt < resMax) {
      if (dut.io.out.valid.peek.litToBoolean) {
        //Slices of KE-matrix that goes into this result
        val slices = KE.slice((resCnt % smpr) * KE_SIZE, ((resCnt % smpr) + 1) * KE_SIZE)
        //B-values used for this result
        val b = arr(rs1 * VREG_SLOT_WIDTH + resCnt / smpr)(0)

        val outVals = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
        //For each slice, add the product of all slice values and the corresponding b-value
        for (i <- 0 until KE_SIZE) {
          for (j <- 0 until NUM_PROCELEM) {
            outVals(j) = fixedAdd(outVals(j), fixedMul(double2fixed(slices(i)(j)).S, b(i)))
          }
        }
        for (j <- 0 until NUM_PROCELEM) {
          dut.io.out.res(j).expect(outVals(j))
        }
        dut.io.out.dest.rd.expect((Rd + resCnt / smpr).U)
        dut.io.out.dest.subvec.expect((resCnt % smpr).U)
        resCnt += 1
      }
      i += 1
      dut.clock.step()
    }
    assert(resCnt == resMax)
    dut.io.exctrl.count.expect(0.U)
    print(s"Final i value $i\n")
  }

  /**
   * Generates 4 random instructions of the given type and pokes them onto the DUT
   * @param dut the DUT
   */
  def genAndPoke(dut: DecodeExecute, mod: RtypeMod.Type): Array[RtypeInstruction] = {
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
   * Calculates the result of an ordinary arithmetic instruction
   * @param op The DUT
   * @param a The first value / numerator
   * @param b The second value / denominator
   * @return
   */
  def calculateRes(op: Opcode.Type, a: SInt, b: SInt): SInt = {
    val ol = op.litValue
    var res = 0.S
    //For some reason, we need to use if/else here, instead of ordinary matching
    if (ol == ADD.litValue) {
      res = fixedAdd(a, b)
    } else if (ol == SUB.litValue()) {
      res = fixedSub(a, b)
    } else if (ol == MUL.litValue) {
      res = fixedMul(a, b)
    } else if (ol == DIV.litValue) {
      res = fixedDiv(a, b)
    } else {
      throw new IllegalArgumentException("Unknown opcode")
    }
    res
  }

  def loadInstructions(ops: Array[Bundle with Instruction], dut: DecodeExecute): Unit = {
    dut.io.idctrl.iload.poke(true.B)
    for(op <- ops) {
      dut.io.in.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.io.idctrl.iload.poke(false.B)
    dut.clock.step()
  }

  def genRtype(op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    val rand = scala.util.Random

    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  /**
   * Computes and prints the random seed to be used for a test
   * @param name The name of the test
   */
  def seed(name: String): Unit = {
    val seed = scala.util.Random.nextLong()
    scala.util.Random.setSeed(seed)
    print(s"$name. Using seed $seed\n")
  }

  it should "not stall the decoder when like operations are processed" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    NUM_PROCELEM = 4
    KE_SIZE = 16
    FIXED_WIDTH = 20
    INT_WIDTH = 10
    FRAC_WIDTH = 9
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //This is a simple test where we first use two like instructions to ensure no stalls,
      //and then use a final, different instruction to check that it stalls
      val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR)
      val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.ELEM)
      val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.ELEM)
      val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)

      val addvv = genRtype(ADD, RtypeMod.VV)
      val addvv2 = genRtype(ADD, RtypeMod.VV)
      val subvv = genRtype(SUB, RtypeMod.VV)

      val ops = Array(istart, estart, addvv, addvv2, subvv, eend, iend)
      loadInstructions(ops, dut)

      var fc = 0
      var i = 0
      var execThread = dut.io.idctrl.execThread.peek.litValue.toInt
      while(i < 100 && fc < 2) {
        execThread = dut.io.idctrl.execThread.peek.litValue.toInt
        if(dut.io.idctrl.threadCtrl(execThread).finalCycle.peek.litToBoolean) {fc += 1}
        dut.clock.step()
        i += 1
      }
      assert(fc == 2)
      fc = 0
      i = 0
      while(i < 20 && fc < 1) {
      //Should we stall the decoder or the exec thread?
        //We want to see the decoder/exec being stalled
        execThread = dut.io.idctrl.execThread.peek.litValue.toInt
        if(dut.io.idstall.peek.litToBoolean) {
          fc = 1
        }
        i += 1
        dut.clock.step()
      }
      assert(fc == 1)
    }
  }

  it should "decode and execute VV instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 24
    INT_WIDTH = 8
    FRAC_WIDTH = 15
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("VV decode execute")
      testSameMod(dut, RtypeMod.VV)
    }
  }

  it should "decode and execute XV instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 20
    INT_WIDTH = 10
    FRAC_WIDTH = 9
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"XV decode/execute. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testSameMod(dut, RtypeMod.XV)
    }
  }

  it should "decode and execute XX instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 20
    INT_WIDTH = 10
    FRAC_WIDTH = 9
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"XX decode/execute. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testSameMod(dut, RtypeMod.XX)
    }
  }

  it should "decode and execute SV instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 20
    INT_WIDTH = 10
    FRAC_WIDTH = 9
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"SV decode/execute. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testSameMod(dut, RtypeMod.SV)
    }
  }

  it should "decode and execute SS instructions" in {
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 20
    INT_WIDTH = 10
    FRAC_WIDTH = 9
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val seed = scala.util.Random.nextLong()
      print(s"SS decode/execute. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testSameMod(dut, RtypeMod.SS)
    }
  }

  it should "decode and execute specific VV instructions" in {
    //These seeds have previously made the test fail. Used for regression testing
    SIMULATION = true
    NUM_VECTOR_REGISTERS = 16
    VECTOR_REGISTER_DEPTH = 16
    KE_SIZE = 16
    NUM_PROCELEM = 4
    FIXED_WIDTH = 24
    INT_WIDTH = 8
    FRAC_WIDTH = 15
    Config.checkRequirements()
    val seeds = Array(6838063735844486541L, -3695747970121693403L)
    for(seed  <- seeds) {
      test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        print(s"VV, specific seed. Using seed ${seed}\n")
        scala.util.Random.setSeed(seed)
        testSameMod(dut, RtypeMod.VV)
      }
    }
  }

  it should "decode and execute MVP instructions" in {
    SIMULATION = true
    KE_SIZE = 6
    NUM_PROCELEM = 2
    VECTOR_REGISTER_DEPTH = 6
    NUM_VECTOR_REGISTERS = 8
    FIXED_WIDTH = 20
    INT_WIDTH = 18
    FRAC_WIDTH = 1
    Config.checkRequirements()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val seed = scala.util.Random.nextLong()
      print(s"MVP decode/execute. Using seed ${seed}\n")
      scala.util.Random.setSeed(seed)
      testMVP(dut)
    }
  }


}
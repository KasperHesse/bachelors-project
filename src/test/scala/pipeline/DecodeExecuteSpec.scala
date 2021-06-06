package pipeline

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

class DecodeExecuteSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode execute stages"

  var MAClength: Int = _

  /**
   * Used to step through and expect the values output from the decode-execute stage when simulating
   * @param dut the DUT
   * @param instr The instruction which generated the expected outputs
   */
  def expectVVvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val vReg = dut.decode.threads(0).vRegFile.arr
    val subvectorsPerRegister = VREG_DEPTH/NUM_PROCELEM

    val op: Opcode.Type = instr.op
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd.litValue().toInt

    if(op.litValue == MAC.litValue) {
      //Example: Each register is 16(VECTOR_REGISTER_DEPTH) elements deep, and 4(NUM_PROCELEM) elements are processed on each clock cycle.
      //Hence, it takes 4 cycles to clear each register * 4(VREG_SLOT_WIDTH) registers per slot = 16 cycles for each vreg slot = 64(ELEMS_PER_VSLOT) values processed
      //After 16 cycles, we swap to the other thread, repeating our calculations
      //If eg. NDOF=128, we require one full pass through each thread before 128 values have been processed
      //The result is 4(NUM_PROCELEM) sub-sums, which totalled would give us the final sum.

      val tempResults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
      //Generate results from traversing the two full vectors
      for (s <- 0 until VREG_SLOT_WIDTH) {
        for (u <- 0 until subvectorsPerRegister) {
          for (k <- 0 until NUM_PROCELEM) {
            val sv1 = vReg(rs1 * VREG_SLOT_WIDTH + s)(u * NUM_PROCELEM + k)
            val sv2 = vReg(rs2 * VREG_SLOT_WIDTH + s)(u * NUM_PROCELEM + k)
            tempResults(k) = fixedAdd(tempResults(k), fixedMul(sv1, sv2))
          }
        }
      }
      //In simulation, values in regs don't change. Simply multiply this value by itself to obtain the final result
      val multiplier = double2fixed(MAClength / ELEMS_PER_VSLOT).S
      for(k <- 0 until NUM_PROCELEM) {
        tempResults(k) = fixedMul(multiplier, tempResults(k))
        dut.io.out.res(k).expect(tempResults(k))
      }
      dut.io.out.dest.reg.expect(instr.rd)
      dut.io.out.dest.rf.expect(RegisterFileType.SREG)
      dut.clock.step()

    } else {
      for (s <- 0 until VREG_SLOT_WIDTH) {
        for (i <- 0 until VREG_DEPTH by NUM_PROCELEM) {
          //Extract operands used (a[0] and b[0] in each iteration
          val op1 = vReg(s + rs1 * VREG_SLOT_WIDTH)(i)
          val op2 = vReg(s + rs2 * VREG_SLOT_WIDTH)(i)
          val res = calculateRes(instr, op1, op2)
          assert(math.abs(fixed2double((dut.io.out.res(0).peek.litValue - res.litValue).toLong)) < 1e-2)
          dut.io.out.dest.reg.expect((rd*VREG_SLOT_WIDTH+s).U)
          dut.io.out.dest.subvec.expect((i / NUM_PROCELEM).U)
          dut.clock.step()
        }
      }
    }
  }

  def expectXVvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd.litValue().toInt
    val subvecsPerVreg = VREG_DEPTH/NUM_PROCELEM
    val vReg = dut.decode.threads(0).vRegFile.arr
    val xReg = dut.decode.threads(0).xRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      val op1 = xReg(rs1)(s)
      for(i <- 0 until subvecsPerVreg) {
        for (j <- 0 until NUM_PROCELEM) {

          val op2 = vReg(s+rs2*VREG_SLOT_WIDTH)(i*NUM_PROCELEM+j)
          val res = calculateRes(instr, op1, op2)
          assert(math.abs(fixed2double(dut.io.out.res(j).peek) - fixed2double(res)) < 1e-2)
          dut.io.out.dest.reg.expect((rd*VREG_SLOT_WIDTH + s).U)
          dut.io.out.dest.subvec.expect(i.U)
        }
        dut.io.out.dest.rf.expect(RegisterFileType.VREG)
        dut.clock.step()
      }
    }
  }

  def expectXXvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd
    val xReg = dut.decode.threads(0).xRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = xReg(rs1)(i)
      val op2 = xReg(rs2)(i)
      val res = calculateRes(instr, op1, op2)
      assert(math.abs(fixed2double((dut.io.out.res(i).peek.litValue - res.litValue).toLong)) < 1e-2)
      dut.io.out.dest.reg.expect(rd)
      dut.io.out.dest.subvec.expect(0.U)
      dut.io.out.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectSVvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd.litValue.toInt
    val sReg = dut.decode.sRegFile.arr
    val vReg = dut.decode.threads(0).vRegFile.arr

    if (instr.op.litValue == MAC.litValue) {
      //MAC.SV instruction (SUM instruction)
      val tempResults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))

      for(s <- 0 until VREG_SLOT_WIDTH) {
        for (u <- 0 until SUBVECTORS_PER_VREG) {
          for(k <- 0 until NUM_PROCELEM) {
            val a = sReg(rs1)
            val b = vReg(s + rs2 * VREG_SLOT_WIDTH)(u * NUM_PROCELEM + k)
            tempResults(k) = fixedAdd(tempResults(k), fixedMul(a,b))
          }
        }
      }
      val multiplier = double2fixed(MAClength / ELEMS_PER_VSLOT).S
      for(k <- 0 until NUM_PROCELEM) {
        tempResults(k) = fixedMul(multiplier, tempResults(k))
        dut.io.out.res(k).expect(tempResults(k))
      }
      dut.io.out.dest.reg.expect(instr.rd)
      dut.io.out.dest.rf.expect(RegisterFileType.SREG)
      dut.clock.step()
    } else {
      val a = sReg(rs1) //first operand from S registers
      for (s <- 0 until VREG_SLOT_WIDTH) {
        for (i <- 0 until SUBVECTORS_PER_VREG) {
          for (j <- 0 until NUM_PROCELEM) {
            val b = vReg(s + rs2 * VREG_SLOT_WIDTH)(i * NUM_PROCELEM + j)
            val res = calculateRes(instr, a, b)
            assert(math.abs(fixed2double((dut.io.out.res(j).peek.litValue - res.litValue).toLong)) < 1e-2)
            dut.io.out.dest.rf.expect(RegisterFileType.VREG)
            dut.io.out.dest.reg.expect((s + rd * VREG_SLOT_WIDTH).U)
            dut.io.out.dest.subvec.expect(i.U)
          }
          dut.clock.step()
        }
      }
    }
  }

  def expectSXvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd
    val op = instr.op
    val xReg = dut.decode.threads(0).xRegFile.arr
    val sReg = dut.decode.sRegFile.arr
    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = xReg(rs2)(i)
      val res = calculateRes(instr, op1, op2)
      assert(math.abs(fixed2double((dut.io.out.res(i).peek.litValue - res.litValue).toLong)) < 1e-2)
      dut.io.out.dest.reg.expect(rd)
      dut.io.out.dest.subvec.expect(0.U)
      dut.io.out.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }


  def expectSSvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rd = instr.rd
    val sReg = dut.decode.sRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      val op1 = sReg(rs1)
      val op2 = sReg(rs2)
      val res = calculateRes(instr, op1, op2)
      assert(math.abs(fixed2double((dut.io.out.res(i).peek.litValue - res.litValue).toLong)) < 1e-2)
      dut.io.out.dest.reg.expect(rd)
      dut.io.out.dest.subvec.expect(0.U)
      dut.io.out.dest.rf.expect(RegisterFileType.SREG)
    }
    dut.clock.step()
  }

  def expectKVvalues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val smpr = KE_SIZE / NUM_PROCELEM //Sub-matrices per row of KE matrix
    val KE = dut.decode.threads(0).KE.KE
    val arr = dut.decode.threads(0).vRegFile.arr
    val rs1 = instr.rs1.litValue.toInt
    val Rd = instr.rd.litValue.toInt

    var i = 0
    var resCnt = 0
    var resMax = KE_SIZE //KE_SIZE/NUM_PROCELEM (eg 24/8=3) outputs per. matrix-vector product
    //Multiplied by VREG_SLOT_WIDTH outputs per slot. Since VREG_SLOT_WIDTH==NUM_PROCELEM, we get KE_SIZE total results
    while (i < 600 && resCnt < resMax) {
      if (dut.io.out.valid.peek.litToBoolean) {
        //Slices of KE-matrix that goes into this result
        val slices = KE.slice((resCnt % smpr) * KE_SIZE, ((resCnt % smpr) + 1) * KE_SIZE)
        //B-values used for this result
        val b = arr(rs1 * VREG_SLOT_WIDTH + resCnt / smpr)

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
        dut.io.out.dest.reg.expect((Rd*VREG_SLOT_WIDTH + resCnt / smpr).U)
        dut.io.out.dest.subvec.expect((resCnt % smpr).U)
        resCnt += 1
      }
      i += 1
      dut.clock.step()
    }
    assert(resCnt == resMax)
  }

  /**
   * Method that calls the correct version of expectZZvalues, where ZZ is some combination of X, V and S
   *
   * @param dut The DUT
   * @param instr The instruction to expect results from
   */
  def expectValues(dut: DecodeExecute, instr: RtypeInstruction): Unit = {
    val mod = instr.mod.litValue
//    print(s"Expecting for mod ${instr.mod}\n")
    if(mod == RtypeMod.VV.litValue) {
      expectVVvalues(dut, instr)
    } else if (mod == RtypeMod.XV.litValue()) {
      expectXVvalues(dut, instr)
    } else if (mod == RtypeMod.XX.litValue()) {
      expectXXvalues(dut, instr)
    } else if (mod == RtypeMod.SV.litValue()) {
      expectSVvalues(dut, instr)
    } else if (mod == RtypeMod.SX.litValue()) {
      expectSXvalues(dut, instr)
    } else if (mod == RtypeMod.SS.litValue()) {
      expectSSvalues(dut, instr)
    } else if(mod == RtypeMod.KV.litValue()) {
      expectKVvalues(dut, instr)
    } else {
        throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }

  /**
   * Tests an instruction mix where all instructions only run on one thread
   * @param dut The DUT
   * @param instrs The executable instructions that are run
   */
  def test(dut: DecodeExecute, instrs: Array[RtypeInstruction]): Unit = {
    test(dut, instrs, OtypeLen.SINGLE)
  }

  /**
   * Tests an instruction mix with a given length
   * @param dut The DUT
   * @param instrs The executable instructions
   * @param len The length of this instruction
   */
  def test(dut: DecodeExecute, instrs: Array[RtypeInstruction], len: OtypeLen.Type): Unit = {
    import OtypeLen._
    var maxProgress = 0
    var progressIncr = 0
    //How many thread swaps should we anticipate?
    if(len.litValue == SINGLE.litValue) {
      maxProgress = 1
      progressIncr = 1
    } else if (len.litValue == NDOF.litValue) {
      maxProgress = NDOFLENGTH
      progressIncr = ELEMS_PER_VSLOT
    } else if (len.litValue() == NELEMVEC.litValue()) {
      maxProgress = NELEMLENGTH
      progressIncr = ELEMS_PER_VSLOT
    } else {
      throw new IllegalArgumentException("Unable to decode length")
    }

    var progress = 0 //How many elements have been processed
    var i = 0
    while(progress < maxProgress && i < 300) {
      var resCnt = 0 //How many instructions of the current packet have been processed
      print(s"resCnt(${instrs.length}): ")
      while(resCnt < instrs.length && i < 300) {
        if(dut.io.out.valid.peek.litToBoolean) {
          //MAC.VV instructions only output on the final cycle of that packet. Skip them while working towards the final outputs
          if(instrs(resCnt).op.litValue == MAC.litValue && instrs(resCnt).mod.litValue == RtypeMod.VV.litValue && progress != (maxProgress-progressIncr)) {
            resCnt += 1
          }
          expectValues(dut, instrs(resCnt))
          resCnt += 1
          print(s"$resCnt ")
        } else {
          dut.clock.step()
        }
        i += 1
      }
      assert(resCnt == instrs.length) //End of loop through packet
      progress += progressIncr
      print(s"\nProgress: $progress/$maxProgress\n")
    }
    dut.clock.step(4)
    assert(progress == maxProgress)
    dut.io.exctrl.empty.expect(true.B)
    dut.io.idctrl.threadCtrl(0).stateUint.expect(ThreadState.sIdle.litValue().U)
    dut.io.idctrl.threadCtrl(1).stateUint.expect(ThreadState.sIdle.litValue().U)
  }

  /**
   * Generates 4 instructions of the same Rtype and pokes them onto the DUT
   * @param dut the DUT
   */
  def genAndPoke(dut: DecodeExecute, mod: RtypeMod.Type): Array[RtypeInstruction] = {
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
  def genAndPoke(dut: DecodeExecute): Array[RtypeInstruction] = {
    val instrs = Array.fill(4)(genRtype())
    val ops = wrapInstructions(instrs)
    loadInstructions(ops, dut)

    instrs
  }

  def loadInstructions(ops: Array[Bundle with Instruction], dut: DecodeExecute): Unit = {
    for(op <- ops) {
      dut.io.in.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.clock.step()
  }


  it should "calculate a single MAC instruction on stored values" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("Decode/execute single MAC instruction")
      val instrs = Array(genRtype(MAC, RtypeMod.VV))
      val ops = wrapInstructions(instrs)
      MAClength = ELEMS_PER_VSLOT
      loadInstructions(ops, dut)
      test(dut, instrs)
    }
  }

  it should "calculate a sum (MAC.SV)" in {
    simulationConfig()
    test(new DecodeExecute) {dut =>
      seed("Decode/execute sum instruction")
      val instrs = Array(genRtype(MAC, RtypeMod.SV))
      val ops = wrapInstructions(instrs, OtypeLen.NELEMVEC)
      MAClength = NELEMLENGTH
      loadInstructions(ops, dut)
      test(dut, instrs)
    }
  }

  it should "postpone MAC.VV results until they are ready" in {
    //This test ensures that we can delay MAC result generation until the cycle it is actually present
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("Decode/execute postpone MAC results")
      val instrs = Array(genRtype(MAC, RtypeMod.VV), genRtype(ADD, RtypeMod.VV))
      val ops = wrapInstructions(instrs, OtypeLen.NELEMVEC)
      MAClength = NELEMLENGTH
      loadInstructions(ops, dut)
      test(dut, instrs, OtypeLen.NELEMVEC)
    }
  }

  it should "decode and execute a dot product (MAC.VV)" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.setTimeout(500)
      seed("Decode/execute dot product")
      val instrs = Array(genRtype(MAC, RtypeMod.VV))
      val ops = wrapInstructions(instrs, OtypeLen.NDOF)
      MAClength = NDOFLENGTH
      loadInstructions(ops, dut)
      test(dut, instrs) //Testing with length SINGLE since we only expect one output. It will still swap threads
    }
  }

  it should "operate on an NELEM long vector" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      //When processing element-wise, we can load down ELEMS_PER_VSLOT elements per iteration
      // On each thread swap, progress counter should increment by that amount
      seed("Decode/execute NELEM long instruction")
      val instrs = Array(genRtype(MAC, RtypeMod.KV), genRtype(MUL, RtypeMod.XX))
      val ops = wrapInstructions(instrs, OtypeLen.NELEMVEC)
      loadInstructions(ops, dut)
      test(dut, instrs, OtypeLen.NELEMVEC)
    }
  }

  it should "not stall the decoder when like operations are processed" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      //This is a simple test where we first use two like instructions to ensure no stalls,
      //and then use a final, different instruction to check that it stalls
      val addvv = genRtype(ADD, RtypeMod.VV)
      val addvv2 = genRtype(ADD, RtypeMod.VV)
      val subvv = genRtype(SUB, RtypeMod.VV)
      val instrs = Array(addvv, addvv2, subvv)
      val ops = wrapInstructions(instrs)

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
        execThread = dut.io.idctrl.execThread.peek.litValue.toInt
        if(dut.io.execStall.peek.litToBoolean) {
          fc = 1
        }
        i += 1
        dut.clock.step()
      }
      assert(fc == 1)
    }
  }

  it should "decode and execute MVP instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("Decode/execute MVP")
      val instrs = Array(genRtype(MAC, RtypeMod.KV))
      val ops = wrapInstructions(instrs)
      loadInstructions(ops, dut)
      test(dut, instrs)
    }
  }

  it should "decode and execute VV instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("VV decode execute")
      val instrs = genAndPoke(dut, RtypeMod.VV)
      test(dut, instrs)
    }
  }

  it should "decode and execute XV instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("XV decode/execute")
      val instrs = genAndPoke(dut, RtypeMod.XV)
      test(dut, instrs)
    }
  }

  it should "decode execute XV instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("XV decode/execute", Some(5847713380284279438L))
      val instrs = genAndPoke(dut, RtypeMod.XV)
      test(dut, instrs)
    }
  }

  it should "decode and execute XX instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("XX decode/execute")
      val instrs = genAndPoke(dut, RtypeMod.XX)
      test(dut, instrs)
    }
  }

  it should "decode and execute SV instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("SV decode/execute")
      val instrs = genAndPoke(dut, RtypeMod.SV)
      test(dut, instrs)
    }
  }

  it should "decode and execute SX instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("SX decode/execute")
      val instrs = genAndPoke(dut, RtypeMod.SX)
      test(dut, instrs)
    }
  }

  it should "decode and execute SS instructions" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      seed("SV decode/execute")
      val instrs = genAndPoke(dut, RtypeMod.SV)
      test(dut, instrs)
    }
  }

  it should "decode and execute a random instruction mix" in {
    simulationConfig()
    test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("Decode/execute random mix")
      val instrs = genAndPoke(dut)
      test(dut, instrs)
    }
  }


  it should "decode and execute specific VV instructions" in {
    //These seeds have previously made the test fail. Used for regression testing
    simulationConfig()
//    val seeds = Array(6838063735844486541L, -3695747970121693403L)
//    val seeds = Array(6838063735844486541L)
    val seed = 6838063735844486541L
//    for(seed  <- seeds) {
      test(new DecodeExecute).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
        print(s"VV, specific seed. Using seed ${seed}\n")
        scala.util.Random.setSeed(seed)
        val instrs = genAndPoke(dut, RtypeMod.VV)
        test(dut, instrs)
//      }
    }
  }






}
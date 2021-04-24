package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.{KEWrapper, Opcode}
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import utils.Config._
import Opcode._
import utils.Config

class DecExWbSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  var sReg: Array[SInt] = _
  var xReg: Array[Array[Array[SInt]]] = _
  var vReg: Array[Array[Array[SInt]]] = _
  var MAClength: Int = _


  /**
   * Loads an instruction packet into the DUT
   * @param ops The instruction packet
   * @param dut The DUT
   */
  def loadInstructions(ops: Array[Bundle with Instruction], dut: DecExWb): Unit = {
    require(ops.length <= 16, "Instruction buffer can only hold 16 instructions total")
    for(op <- ops) {
      dut.io.in.instr.poke(op.toUInt())
      dut.clock.step()
    }
    dut.clock.step()
  }

  /**
   * Waits until the result of the given instruction is presented on the output of the writebacks stage
   * @param dut The DUT
   * @param instr The instruction to wait on
   */
  def waitForResult(dut: DecExWb, instr: RtypeInstruction): Unit = {
    while(!dut.io.wb.we.peek.litToBoolean){
      dut.clock.step()
    }
  }

  /**
   * Sets the references to the testers's [[sReg]], [[xReg]] and [[vReg]] variables to match DUT's starting values
   * @param dut the DUT
   */
  def setRegisters(dut: DecExWb): Unit = {
    sReg = dut.decode.sRegFile.arr
    xReg = dut.decode.threads(0).xRegFile.arr
    vReg = dut.decode.threads(0).vRegFile.arr
  }


  /**
   * Returns the [[RegisterFileType]] of the destination of the given instruction
   * @param instr The instruction to be parsed
   * @return The register file type of the register file where the result will be stored
   */
  def getResultRegisterType(instr: RtypeInstruction): RegisterFileType.Type = {
    import RtypeMod._
    import RegisterFileType._
    import Opcode._

    val mod = instr.mod.litValue
    if(mod == SS.litValue || (mod == SV.litValue && instr.op.litValue == MAC.litValue)) {
      SREG
    } else if (Seq(SX.litValue, XX.litValue).contains(mod)) {
      XREG
    } else if (Seq(XV.litValue, SV.litValue, KV.litValue).contains(mod)) {
      VREG
    } else if (mod == VV.litValue) {
      if(instr.op.litValue == MAC.litValue) {
        SREG
      } else {
        VREG
      }
    } else {
      throw new IllegalArgumentException("Could not calculate result register type")
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier VV. Does not calculate dot products.
   * For that, see [[calculateDOTresult]]
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateVVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    for(i <- 0 until VREG_DEPTH) {
      val a = vReg(rs1*VREG_SLOT_WIDTH+rdOffset)(0)(i)
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(0)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XV
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateXVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val a = xReg(rs1)(0)(rdOffset)
    for(i <- 0 until VREG_DEPTH) {
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(0)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SV. Does not calculate summations (mac.sv).
   * For that, see [[calculateDOTresult]]
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateSVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    for(i <- 0 until VREG_DEPTH) {
      val a = sReg(rs1)
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(0)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateXXresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for (i <- 0 until NUM_PROCELEM) {
      val a = xReg(rs1)(0)(i)
      val b = xReg(rs2)(0)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSXresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for (i <- 0 until NUM_PROCELEM) {
      val a = sReg(rs1)
      val b = xReg(rs2)(0)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SS
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSSresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for (i <- 0 until NUM_PROCELEM) {
      val a = sReg(rs1)
      val b = sReg(rs2)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier KV
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateKVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt): Unit = {
    val KE = KEWrapper.getKEMatrix()

    val rs1 = instr.rs1.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    //Zero out results
    for(i <- 0 until VREG_DEPTH) {
      results(i) = 0.S(FIXED_WIDTH.W)
    }
    for(i <- 0 until KE_SIZE) {
      for(j <- 0 until KE_SIZE) {
        val a = double2fixed(KE(i)(j)).S
        val b = vReg(rs1*VREG_SLOT_WIDTH + rdOffset)(0)(j)
        results(i) = fixedAdd(results(i), fixedMul(a, b))
      }
    }
  }

  /**
   * Calculates the result of a dot product (mac.vv)
   * @note Should only be used when this instruction is the only instruction in the mix. Does not take into account
   *       register values that are updated throughout execution
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateDOTresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val tempResults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
    //Generate results from traversing the two full vectors
    for (s <- 0 until VREG_SLOT_WIDTH) {
      for (u <- 0 until SUBVECTORS_PER_VREG) {
        for (k <- 0 until NUM_PROCELEM) {
          val a = vReg(rs1 * VREG_SLOT_WIDTH + s)(0)(u * NUM_PROCELEM + k)
          val b = vReg(rs2 * VREG_SLOT_WIDTH + s)(0)(u * NUM_PROCELEM + k)
          tempResults(k) = fixedAdd(tempResults(k), fixedMul(a, b))
        }
      }
    }
    //Multiply by itself to simulate multiple traversals
    val multiplier = double2fixed(MAClength / ELEMS_PER_VSLOT).S
    for(k <- 0 until NUM_PROCELEM) {
      tempResults(k) = fixedMul(multiplier, tempResults(k))
    }
    //Reduce down to one result
    val result = tempResults.reduce((a,b) => fixedAdd(a,b))
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = result
    }
    for(i <- NUM_PROCELEM until VREG_DEPTH) {
      results(i) = 0.S
    }
  }

  /**
   * Calculates the result of a sum instruction (mac.sv)
   * @note Should only be used when this instruction is the only instruction in the mix. Does not take into account
   *       register values that are updated throughout execution
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSUMresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val tempResults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for (u <- 0 until SUBVECTORS_PER_VREG) {
        for(k <- 0 until NUM_PROCELEM) {
          val a = sReg(rs1)
          val b = vReg(s + rs2 * VREG_SLOT_WIDTH)(0)(u * NUM_PROCELEM + k)
          tempResults(k) = fixedAdd(tempResults(k), fixedMul(a,b))
        }
      }
    }
    val multiplier = double2fixed(MAClength / ELEMS_PER_VSLOT).S
    for(k <- 0 until NUM_PROCELEM) {
      tempResults(k) = fixedMul(multiplier, tempResults(k))
    }
    //Reduce down to one result
    val result = tempResults.reduce((a,b) => fixedAdd(a,b))
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = result
    }
    for(i <- NUM_PROCELEM until VREG_DEPTH) {
      results(i) = 0.S
    }
  }

  /**
   * Expects the output of an instruction going into vector register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectVREG(dut: DecExWb, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(instr.op.litValue() == MAC.litValue && mod == RtypeMod.KV.litValue) {
      calculateKVresult(instr, results, dut.io.wb.rd.reg.peek)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, dut.io.wb.rd.reg.peek)
    } else if (mod == RtypeMod.XV.litValue) {
      calculateXVresult(instr, results, dut.io.wb.rd.reg.peek)
    } else if (mod == RtypeMod.SV.litValue) {
      calculateSVresult(instr, results, dut.io.wb.rd.reg.peek)
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
    dut.io.wb.rd.rf.expect(RegisterFileType.VREG)
    for(i <- 0 until VREG_DEPTH) {
      assert(math.abs(fixed2double((dut.io.wb.wrData(i).peek.litValue - results(i).litValue).toLong)) < 1e-2)
      results(i) = dut.io.wb.wrData(i).peek //to avoid any incremental changes
    }
  }

  /**
   * Updates the simulation vector register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateVREG(dut: DecExWb, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rd = instr.rd.litValue.toInt
    val rdOffset = dut.io.wb.rd.reg.peek.litValue.toInt % VREG_SLOT_WIDTH
    for (j <- 0 until VREG_DEPTH) {
      vReg(rd * VREG_SLOT_WIDTH + rdOffset)(0)(j) = results(j)
    }
  }

  /**
   * Expects the output of an instruction going into x-vector register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectXREG(dut: DecExWb, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    if(instr.mod.litValue == RtypeMod.SX.litValue) {
      calculateSXresult(instr, results)
    } else if (instr.mod.litValue == RtypeMod.XX.litValue) {
      calculateXXresult(instr, results)
    } else {
      throw new IllegalArgumentException("Cannot decode modtype with result being xreg")
    }
    dut.io.wb.rd.rf.expect(RegisterFileType.XREG)
    dut.io.wb.rd.subvec.expect(0.U)
    for (i <- 0 until NUM_PROCELEM) {
      assert(math.abs(fixed2double((dut.io.wb.wrData(i).peek.litValue - results(i).litValue).toLong)) < 1e-2)
      results(i) = dut.io.wb.wrData(i).peek
    }
    for (i <- NUM_PROCELEM until VREG_DEPTH) {
      dut.io.wb.wrData(i).expect(0.S)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateXREG(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rd = instr.rd.litValue.toInt
    for(i <- 0 until NUM_PROCELEM) {
      xReg(rd)(0)(i) = results(i)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateSREG(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rd = instr.rd.litValue.toInt
    if(rd != 0) { sReg(rd) = results(0) }
  }

  /**
   * Expects the output of an instruction going into scalar register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectSREG(dut: DecExWb, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.SS.litValue) {
      calculateSSresult(instr, results)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateDOTresult(instr, results)
    } else if(mod == RtypeMod.SV.litValue) {
      calculateSUMresult(instr, results)
    } else {
      throw new IllegalArgumentException("R-type mod not recognized")
    }
    dut.io.wb.rd.rf.expect(RegisterFileType.SREG)
    dut.io.wb.rd.subvec.expect(0.U)
    for(i <- 0 until NUM_PROCELEM) {
      assert(math.abs(fixed2double((dut.io.wb.wrData(i).peek.litValue - results(i).litValue).toLong)) < 1e-2)
      results(i) = dut.io.wb.wrData(i).peek
    }
    for (i <- NUM_PROCELEM until VREG_DEPTH) {
      dut.io.wb.wrData(i).expect(0.S)
    }
  }


  /**
   * Checks whether the results returned by writeback module match our expectations, and updates the simulation's
   * register files to match the updated state of the DUT.
   * @param dut The DUT
   * @param results Array holding the result buffer.
   * @param instr The instruction to expect the output of
   */
  def expectAndUpdate(dut: DecExWb, results: Array[SInt], instr: RtypeInstruction): Unit = {
    import RegisterFileType._
    val rf = getResultRegisterType(instr).litValue
    if(rf == VREG.litValue) {
      for(i <- 0 until VREG_SLOT_WIDTH) { //We need to update all vreg slots
        waitForResult(dut, instr)
        expectVREG(dut, instr, results)
        updateVREG(dut, instr, results)
        if (i < VREG_SLOT_WIDTH-1) dut.clock.step() //Don't step after final result, this happens in test()
      }
    } else if (rf == XREG.litValue) {
      expectXREG(dut, instr, results)
      updateXREG(instr, results)

    } else if (rf == SREG.litValue) {
      expectSREG(dut, instr, results)
      updateSREG(instr, results)
    } else {
      throw new IllegalArgumentException("Unknown register file type")
    }
  }

  /**
   * The main test function for this module. Load instructions into the DUT, awaits results and updates register files
   * @param dut The DUT
   * @param instrs The instructions to be used
   */
  def test(dut: DecExWb, instrs: Array[RtypeInstruction]): Unit = {
    val ops = wrapInstructions(instrs)
    loadInstructions(ops, dut)
    setRegisters(dut)
    val results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))

    for(instr <- instrs) {
      println(instr)
      //Every time a result is generated, store it. Only check VREG results for correctness every once in a while?
      waitForResult(dut, instr)
      //Update variables
      expectAndUpdate(dut, results, instr)
      println("Instruction done")
      dut.clock.step()
    }
  }

  "DecExWbSpec" should "execute a VREG instruction and store the result" in {
    genericConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb vreg store result")
      val instrs = Array(RtypeInstruction(1, 1, 3, DIV, RtypeMod.VV), RtypeInstruction(1, 1, 3, DIV, RtypeMod.VV))
      test(dut, instrs)
    }
  }

  "DecExWbSpec" should "execute an XREG instruction and store the result" in {
    genericConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb xreg store result")
      val instrs = Array(RtypeInstruction(1, 2, 1, ADD, RtypeMod.SX), RtypeInstruction(2, 1, 1, ADD, RtypeMod.SX))
      test(dut, instrs)
    }
  }

  "DecExWbSpec" should "execute an SREG instruction and store the result" in {
    genericConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb SREG store result")
      val instrs = Array(RtypeInstruction(0, 0, 1, ADD, RtypeMod.SS), RtypeInstruction(0, 0, 1, ADD, RtypeMod.SS))
      test(dut, instrs)
    }
  }

  "DecExWbSpec" should "execute a random instruction mix" in {
    genericConfig()
    test(new DecExWb) {dut =>
      seed("DecExWb random mix")
      val instrs = Array.fill(8)(genRtype())
      test(dut,instrs)
    }
  }

  "DecExWbSpec" should "execute a dot product and store the result" in {
    genericConfig()
    test(new DecExWb) { dut =>
      seed("DecExWb dot product")
      val instrs = Array(genRtype(MAC, RtypeMod.VV))
      val len = NDOF
      val ops = wrapInstructions(instrs, len)
      MAClength = if (len % ELEMS_PER_VSLOT == 0) len else (math.floor(len.toDouble/ELEMS_PER_VSLOT).toInt+1)*ELEMS_PER_VSLOT
      loadInstructions(ops, dut)
      setRegisters(dut)
      val results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))

      for(instr <- instrs) {
        //Every time a result is generated, store it. Only check VREG results for correctness every once in a while?
        waitForResult(dut, instr)
        //Update variables
        expectAndUpdate(dut, results, instr)
        dut.clock.step()
      }
    }
  }

  "DecExWbSpec" should "execute a sum and store the result" in {
    genericConfig()
    test(new DecExWb) { dut =>
      seed("DecExWb dot product")
      val instrs = Array(genRtype(MAC, RtypeMod.SV))
      val len = NDOF
      val ops = wrapInstructions(instrs, len)
      MAClength = if (len % ELEMS_PER_VSLOT == 0) len else (math.floor(len.toDouble/ELEMS_PER_VSLOT).toInt+1)*ELEMS_PER_VSLOT
      loadInstructions(ops, dut)
      setRegisters(dut)
      val results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))
      for(instr <- instrs) {
        //Every time a result is generated, store it. Only check VREG results for correctness every once in a while?
        waitForResult(dut, instr)
        //Update variables
        expectAndUpdate(dut, results, instr)
        dut.clock.step()
      }
    }
  }
}

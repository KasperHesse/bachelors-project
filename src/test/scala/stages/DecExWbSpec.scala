package execution

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

class DecExWbSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  var sReg: Array[SInt] = _
  var xReg: Array[Array[SInt]] = _
  var vReg: Array[Array[SInt]] = _
  var MAClength: Int = _

  /**
   * Verifies that two fixed-point numbers represent the same value, to within some margin of error
   * @param a The first value
   * @param b The second value
   * @param delta Maximum allowed deviation (not inclusive)
   */
  def assertEquals(a: SInt, b: SInt, delta: Double = 0.01): Unit = {
    assert(math.abs(fixed2double((a.litValue-b.litValue).toLong)) < delta, s"[a=$a (${fixed2double(a)}), b=$b (${fixed2double(b)})]")
  }


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
   * Waits until the result of the given instruction is presented on the output of the writeback stage
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
          val a = vReg(rs1 * VREG_SLOT_WIDTH + s)(u * NUM_PROCELEM + k)
          val b = vReg(rs2 * VREG_SLOT_WIDTH + s)(u * NUM_PROCELEM + k)
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
          val b = vReg(s + rs2 * VREG_SLOT_WIDTH)(u * NUM_PROCELEM + k)
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
   * Calculates the result of a red.xx instruction
   * @param instr The instruction
   * @param results The results registers
   */
  def calculateRedXXresult(instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for(i <- 0 until XREG_DEPTH) {
      val a = xReg(rs1)(i)
      val b = xReg(rs2)(i)
      results(0) = fixedAdd(results(0), fixedMul(a,b))
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
      calculateKVresult(instr, results, dut.io.wb.rd.reg.peek, vReg)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, dut.io.wb.rd.reg.peek, vReg)
    } else if (mod == RtypeMod.XV.litValue) {
      calculateXVresult(instr, results, dut.io.wb.rd.reg.peek, xReg, vReg)
    } else if (mod == RtypeMod.SV.litValue) {
      calculateSVresult(instr, results, dut.io.wb.rd.reg.peek, sReg, vReg)
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
   * Expects the output of an instruction going into x-vector register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectXREG(dut: DecExWb, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    if(instr.mod.litValue == RtypeMod.SX.litValue) {
      calculateSXresult(instr, results, sReg, xReg)
    } else if (instr.mod.litValue == RtypeMod.XX.litValue) {
      calculateXXresult(instr, results, xReg)
    } else if (instr.mod.litValue == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, 0.U, vReg) //Setting rd == 0 since we know all of the expected output data
    } else {
      throw new IllegalArgumentException("Cannot decode modtype with result being xreg")
    }
    dut.io.wb.rd.rf.expect(RegisterFileType.XREG)
    dut.io.wb.rd.subvec.expect(0.U)
    for (i <- 0 until NUM_PROCELEM) {
      assertEquals(dut.io.wb.wrData(i).peek, results(i))
//      assert(math.abs(fixed2double((dut.io.wb.wrData(i).peek.litValue - results(i).litValue).toLong)) < 1e-2)
      results(i) = dut.io.wb.wrData(i).peek
    }
    for (i <- NUM_PROCELEM until VREG_DEPTH) {
      dut.io.wb.wrData(i).expect(0.S)
    }
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
      calculateSSresult(instr, results, sReg)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateDOTresult(instr, results)
    } else if(mod == RtypeMod.SV.litValue) {
      calculateSUMresult(instr, results)
    } else if(mod == RtypeMod.XX.litValue) {
      calculateRedXXresult(instr, results)
    } else {
      throw new IllegalArgumentException("R-type mod not recognized")
    }
    dut.io.wb.rd.rf.expect(RegisterFileType.SREG)
    dut.io.wb.rd.subvec.expect(0.U)
    assertEquals(dut.io.wb.wrData(0).peek, results(0))
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = dut.io.wb.wrData(i).peek
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
        updateVREG(instr, results, dut.io.wb.rd.reg.peek, vReg)
        if (i < VREG_SLOT_WIDTH-1) dut.clock.step() //Don't step after final result, this happens in test()
      }
    } else if (rf == XREG.litValue) {
      expectXREG(dut, instr, results)
      updateXREG(instr, results, xReg)

    } else if (rf == SREG.litValue) {
      expectSREG(dut, instr, results)
      updateSREG(instr, results, sReg)
    } else {
      throw new IllegalArgumentException("Unknown register file type")
    }
  }

  /**
   * The main test function for this module. Load instructions into the DUT, awaits results and updates register files
   * @param dut The DUT
   * @param instrs The instructions to be used
   */
  def test(dut: DecExWb, instrs: Array[RtypeInstruction], len: OtypeLen.Type = OtypeLen.SINGLE): Unit = {
    val ops = wrapInstructions(instrs, len)
    loadInstructions(ops, dut)
    setRegisters(dut)
    val results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))

    for(instr <- instrs) {
      println(instr)
      //Every time a result is generated, store it.
      waitForResult(dut, instr)
      //Update variables
      expectAndUpdate(dut, results, instr)
      println("Instruction done")
      dut.clock.step()
    }
    dut.io.exctrl.empty.expect(true.B)
    dut.clock.step(5)
  }

  "DecExWbSpec" should "execute a VREG instruction and store the result" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb vreg store result")
      val instrs = Array(RtypeInstruction(1, 1, 3, DIV, RtypeMod.VV), RtypeInstruction(1, 1, 3, DIV, RtypeMod.VV))
//      val instrs = Array(RtypeInstruction(0, 0, 0, MAC, RtypeMod.KV))
      test(dut, instrs)
    }
  }

  "DecExWbSpec" should "execute an XREG instruction and store the result" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb xreg store result")
      val instrs = Array(RtypeInstruction(1, 2, 1, DIV, RtypeMod.SX),  RtypeInstruction(2, 3, 3, ADD, RtypeMod.XX))
      test(dut, instrs)
    }
  }

  it should "execute a red.vv instruction and get the result in an x-register" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb red.vv")
      val instrs = Array(RtypeInstruction(0, 1, 2, RED, RtypeMod.VV))
      test(dut, instrs, OtypeLen.NELEMDOF)
    }
  }

  "DecExWbSpec" should "execute an SREG instruction and store the result" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb SREG store result")
      val instrs = Array(RtypeInstruction(0, 0, 1, ADD, RtypeMod.SS), RtypeInstruction(0, 0, 1, ADD, RtypeMod.SS))
      test(dut, instrs)
    }
  }

  it should "execute a red.xx instruction and get the result as a scalar" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb red.xx", Some(1L))
      val instrs = Array(RtypeInstruction(1, 2, 3, RED, RtypeMod.XX))
      test(dut, instrs, OtypeLen.NELEMSTEP)
    }
  }

  "DecExWbSpec" should "execute a random instruction mix" in {
    simulationConfig()
    test(new DecExWb){dut =>
      seed("DecExWb random mix")
      val instrs = Array.fill(8)(genRtype())
      test(dut,instrs)
    }
  }

  "DecExWbSpec" should "execute a dot product and store the result" in {
    simulationConfig()
    test(new DecExWb) { dut =>
      seed("DecExWb dot product")
      val instrs = Array(genRtype(MAC, RtypeMod.VV))
      val ops = wrapInstructions(instrs, OtypeLen.NDOF)
      MAClength = NDOFLENGTH
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
    simulationConfig()
    test(new DecExWb) { dut =>
      seed("DecExWb dot product")
      val instrs = Array(genRtype(MAC, RtypeMod.SV))
      val len = NDOF
      val ops = wrapInstructions(instrs, OtypeLen.NDOF)
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

  "DecExWbSpec" should "execute immediates" in {
    simulationConfig()
    test(new DecExWb).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      seed("DecExWb immediates", Some(1L))
      val instrs = Array.fill(2)(genRtype(true))
      test(dut, instrs)
    }
  }
}

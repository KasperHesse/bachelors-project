package pipeline

import java.io._

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import vector.KEWrapper
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.{FailedExpectException, WriteVcdAnnotation}
import utils.Config._
import Opcode._
import pipeline.BranchComp._
import utils.Assembler

import scala.collection.mutable.ListBuffer

class ExecutePipelineSpec extends FlatSpec with ChiselScalatestTester with Matchers {

  behavior of "Execute pipeline"

  /** S-register file in decode stage */
  var sReg: Array[SInt] = _
  /** 2D-array holding both x-reg files from both threads */
  var xReg: Array[Array[Array[SInt]]]= _
  /** 2D-array holding both v-reg files from both threads */
  var vReg: Array[Array[Array[SInt]]] = _
  /** ID of executing thread */
  var execThread: Int = _
  /** ID of memory access thread */
  var memThread: Int = _
  /** Length an instruction operating through NDOF/NELEM elements */
  var MAClength: Int = _
  /** Flag indicating if a MAC instruction is the first MAC instruction seen in that iteration */
  var firstMAC: Boolean = true
  /** The number of MAC instructions that have been processed in the current iteration */
  var macCnt: Int = 0


  /**
   * Verifies that two fixed-point numbers represent the same value, to within some margin of error
   * @param a The first value
   * @param b The second value
   * @param delta Maximum allowed deviation (not inclusive)
   */
  def assertEquals(a: SInt, b: SInt, delta: Double = 0.01): Unit = {
    assert(math.abs(fixed2double((a.litValue-b.litValue).toLong)) < delta)
  }

  /**
   * Sets all global variables.
   * This includes references to the testers's [[sReg]], [[xReg]] and [[vReg]] variables
   * @param dut the DUT
   */
  def setGlobals(dut: ExecutePipeline): Unit = {
    sReg = dut.decode.sRegFile.arr
    xReg = Array(dut.decode.threads(0).xRegFile.arr, dut.decode.threads(1).xRegFile.arr)
    vReg = Array(dut.decode.threads(0).vRegFile.arr, dut.decode.threads(1).vRegFile.arr)
    execThread = 0
    memThread = 1
  }

  /**
   * Verifies that the branch outcome of the DUT matches the simulation's expected branch outcome
   * @param dut The DUT
   * @param instr The branch instruction being evaluated
   */
  def verifyBranchOutcome(dut: ExecutePipeline, instr: BtypeInstruction): Unit = {
    val a = sReg(instr.rs1.litValue.toInt)
    val b = sReg(instr.rs2.litValue.toInt)
    val comp = instr.comp
    val branch = branchOutcome(a, b, comp)
    dut.io.idctrl.branch.expect(branch.B)
  }

  /**
   * Finalizes the operation performed in the MAC-units of the pipeline by performing a sum-reduction
   */
  def calculateReducedResult(results: Array[SInt], MACresults: Array[SInt]): Unit = {
    var temp = 0.S(FIXED_WIDTH.W)
    for(v <- MACresults) {
      temp = fixedAdd(temp, v)
    }
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = temp
      MACresults(i) = 0.S(FIXED_WIDTH.W)
    }
  }

  /**
   * Expects the output of an instruction going into vector register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectVREG(dut: ExecutePipeline, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(instr.op.litValue() == MAC.litValue && mod == RtypeMod.KV.litValue) {
      calculateKVresult(instr, results, dut.io.wbout.rd.reg.peek, vReg(execThread))
    } else if(mod == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, dut.io.wbout.rd.reg.peek, vReg(execThread))
    } else if (mod == RtypeMod.XV.litValue) {
      calculateXVresult(instr, results, dut.io.wbout.rd.reg.peek, xReg(execThread), vReg(execThread))
    } else if (mod == RtypeMod.SV.litValue) {
      calculateSVresult(instr, results, dut.io.wbout.rd.reg.peek, sReg, vReg(execThread))
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
    dut.io.wbout.rd.rf.expect(RegisterFileType.VREG)
    assertEquals(dut.io.wbout.wrData(0).peek, results(0))
    for(i <- 0 until VREG_DEPTH) {
      results(i) = dut.io.wbout.wrData(i).peek //to avoid any incremental changes
    }
  }

  /**
   * Expects the output of an instruction going into x-vector register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectXREG(dut: ExecutePipeline, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    if(instr.mod.litValue == RtypeMod.SX.litValue) {
      calculateSXresult(instr, results, sReg, xReg(execThread))
    } else if (instr.mod.litValue == RtypeMod.XX.litValue) {
      calculateXXresult(instr, results, xReg(execThread))
    } else {
      throw new IllegalArgumentException("Cannot decode modtype with result being xreg")
    }
    dut.io.wbout.rd.rf.expect(RegisterFileType.XREG)
    dut.io.wbout.rd.subvec.expect(0.U)
    assertEquals(dut.io.wbout.wrData(0).peek, results(0))
    for (i <- 0 until NUM_PROCELEM) {
      results(i) = dut.io.wbout.wrData(i).peek
    }
    dut.io.wbout.wrData(NUM_PROCELEM).expect(0.S)
  }

  /**
   * Expects the output of an instruction going into scalar register file
   * @param dut The DUT
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectSREG(dut: ExecutePipeline, instr: RtypeInstruction, results: Array[SInt], MACresults: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.SS.litValue) {
      calculateSSresult(instr, results, sReg)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateReducedResult(results, MACresults)
    } else if(mod == RtypeMod.SV.litValue) {
      calculateReducedResult(results, MACresults)
    } else {
      throw new IllegalArgumentException("R-type mod not recognized")
    }
    dut.io.wbout.rd.rf.expect(RegisterFileType.SREG)
    dut.io.wbout.rd.subvec.expect(0.U)
    assertEquals(dut.io.wbout.wrData(0).peek, results(0))
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = dut.io.wbout.wrData(i).peek
    }
    dut.io.wbout.wrData(NUM_PROCELEM).expect(0.S)
  }

  /**
   * Checks whether the result returned by the writeback module matches our expectations.
   * Updates the register files in the DUT with all new values
   * @param dut The DUT
   * @param results Result buffer
   * @param MACresults Result buffer for MAC instructions
   * @param instr Instruction to expect the output of
   */
  def expectAndUpdate(dut: ExecutePipeline, results: Array[SInt], MACresults: Array[SInt], instr: RtypeInstruction): Unit = {
    import RegisterFileType._
    val rf = getResultRegisterType(instr).litValue
    if(rf == VREG.litValue) {
      //KV, VV, XV and SV-instructions generate multiple results. We need to observe all of those results
      for(i <- 0 until VREG_SLOT_WIDTH) {
        while(!dut.io.wbout.we.peek.litToBoolean) {
          //We still need to perform MAC instructions bookkeeping
          dut.clock.step()
          handleMACSVandMACVV(dut, MACresults)
        }
        expectVREG(dut, instr, results)
        updateVREG(instr, results, dut.io.wbout.rd.reg.peek, vReg(execThread))
        if (i < VREG_SLOT_WIDTH-1) {
          dut.clock.step()
          handleMACSVandMACVV(dut, MACresults)
        } //Don't step after final result, this happens in testFun()
      }
    } else if (rf == XREG.litValue) {
      expectXREG(dut, instr, results)
      updateXREG(instr, results, xReg(execThread))
    } else if (rf == SREG.litValue) {
      expectSREG(dut, instr, results, MACresults)
      updateSREG(instr, results, sReg)
    } else {
      throw new IllegalArgumentException("Unknown register file type")
    }
  }

  /**
   * Performs bookkeeping when operating MAC instructions by storing the intermediate MAC results in a buffer
   * until they are used
   * @param dut The DUT
   * @param MACresults The buffer storing MAC calculations until the result is presented
   */
  def handleMACSVandMACVV(dut: ExecutePipeline, MACresults: Array[SInt]): Unit = {
    val isMACinstruction = dut.io.idout.valid.peek().litToBoolean &&
      dut.io.idout.opUInt.peek.litValue == Opcode.MAC.litValue() &&
      dut.io.idout.dest.rfUint.peek.litValue == RegisterFileType.SREG.litValue
    val macLimit = ELEMS_PER_VSLOT / NUM_PROCELEM //Number of elements added to each result register on each MAC iteration
    /*
      When a mac is noticed on the output, the actual values arrive one clock cycle later since the reg file is a syncreadmem
      To support mac instructions, we first notice the mac instruction and set firstMac false
      On subsequent iterations, the first statement (isMacInstruction && !firstMac) will cause the update logic to be executed
      On the final iteration, isMacInstruction will be low (since control signals are ahead by one clock cycle).
      To solve this, we count up, to ensure that the correct number of values have been added together. Once macCnt == macLimit, we reset the mac update logic
     */
    if((isMACinstruction && !firstMAC) || (0 < macCnt && macCnt < macLimit)) {
      for(i <- 0 until NUM_PROCELEM) {
        val a = dut.io.idout.a(i).peek
        val b = if(dut.io.idout.useImm.peek.litToBoolean) dut.io.idout.imm.peek else dut.io.idout.b(i).peek
        MACresults(i) = fixedAdd(MACresults(i), fixedMul(a,b))
      }
      macCnt += 1
    } else if (isMACinstruction && firstMAC) {
      firstMAC = false
    } else if (!isMACinstruction && macCnt >= macLimit) {
      firstMAC = true
      macCnt = 0
    }
  }

  /**
   * Handles all execution logic, such as expecting outputs and updating register files in simulation
   * @param dut the DUT
   * @param iBuffer The instruction buffer being operated on
   */
  def performExecution(dut: ExecutePipeline, iBuffer: InstructionBuffer): Unit = {
    import OtypeLen._

    var maxProgress = 0   //How many elements total should be processed
    var progressIncr = 0  //Elements processed per thread
    var progress = 0      //Elements processed so far
    if(iBuffer.pstart.len.litValue == SINGLE.litValue) {
      maxProgress = 1
      progressIncr = 1
    } else if (iBuffer.pstart.len.litValue == NDOF.litValue){
      maxProgress = NDOFLENGTH
      progressIncr = ELEMS_PER_VSLOT
      MAClength = NDOFLENGTH
    } else if (iBuffer.pstart.len.litValue == NELEMVEC.litValue) {
      maxProgress = NELEMLENGTH
      progressIncr = ELEMS_PER_VSLOT
      MAClength = NELEMLENGTH
    }

    //We need a special check to ensure correct functionality if only instruction is of type mac.vv/mac.sv
    if(iBuffer.exec.length == 1 && iBuffer.exec(0).op.litValue == MAC.litValue &&
      (iBuffer.exec(0).mod.litValue == RtypeMod.SV.litValue || iBuffer.exec(0).mod.litValue() == RtypeMod.VV.litValue)) {
        maxProgress = 1
        progressIncr = 1
    }

    val results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))
    val MACresults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
    while(progress < maxProgress) {
      var instrCnt = 0
      while(instrCnt < iBuffer.exec.length) {
        //Wait until something is presented on decode stage output.
        //If that is a mac.vv or mac.sv instruction, add to temporary result buffer
        handleMACSVandMACVV(dut, MACresults)

        //When things are presented on writeback output, write back into register file
        if(dut.io.wbout.we.peek().litToBoolean) {
          //MAC.VV, MAC.SV instructions only output on the final cycle of that packet. Skip them while working towards the final outputs
          if(iBuffer.exec(instrCnt).op.litValue == MAC.litValue && getResultRegisterType(iBuffer.exec(instrCnt)).litValue == RegisterFileType.SREG.litValue && progress != (maxProgress-progressIncr)) {
            instrCnt += 1
          }
          expectAndUpdate(dut, results, MACresults, iBuffer.exec(instrCnt))
          instrCnt += 1
        }
        dut.clock.step()
      }
      progress += progressIncr
      //Swap executable and memory threads
      val temp = execThread
      execThread = memThread
      memThread = temp
    }
    //Reset exec and memthread
    execThread = 0
    memThread = 1
  }

  /**
   * Fills the instruction buffer with all instructions in the current instruction packet
   * @param dut The DUT
   * @param iBuffer The instruction buffer to be filled
   */
  def fillInstructionBuffer(dut: ExecutePipeline, iBuffer: InstructionBuffer): Unit = {
    var i = 1
    var instr: UInt = 0.U //Default value
    var fmt: InstructionFMT.Type = InstructionFMT.RTYPE //Default value
    do {
      instr = dut.io.fectrl.instr.peek
      fmt = InstructionFMT(instr(7, 6).litValue.toInt)
      if (fmt.litValue == InstructionFMT.OTYPE.litValue) {
        //Do nothing
      } else if (fmt.litValue == InstructionFMT.RTYPE.litValue()) {
        iBuffer.exec += RtypeInstruction(instr)
      } else if (fmt.litValue == InstructionFMT.STYPE.litValue()) {
        iBuffer.store += new StypeInstruction //TODO implement apply(v: UInt): StypeInstruction
      } else {
        throw new IllegalArgumentException("Unable to decode format")
      }
      i += 1
      dut.clock.step()
      //Continue until we get iend instruction
    } while(instr.litValue != OtypeInstruction(OtypeSE.END, OtypePE.PACKET).toUInt().litValue())
  }

  /**
   * Main testing function for the execute pipeline tester. Performs global variables setup
   * @param dut the DUT
   */
  def testFun(dut: ExecutePipeline): Unit = {
    setGlobals(dut)
    assert(dut.io.fectrl.instr.peek.litValue != 0, "Peeked instruction with value 0, did not init memory correctly")
    while(dut.io.fectrl.instr.peek().litValue != 0) {
      //Always snoop on instruction present at fetch stage register, and carry that over to decode stage
      val instr = dut.io.fectrl.instr.peek()
      val fmt = InstructionFMT(instr(7,6).litValue.toInt)
      dut.clock.step() //Step it into decode stage

      if (fmt.litValue == InstructionFMT.BTYPE.litValue) {
        verifyBranchOutcome(dut, BtypeInstruction(instr))
      } else if (fmt.litValue == InstructionFMT.OTYPE.litValue) { //Executable packet
        val iBuffer = new InstructionBuffer
        iBuffer.pstart = OtypeInstruction(instr)
        fillInstructionBuffer(dut, iBuffer)
        performExecution(dut, iBuffer)
        //Wait until all threads are idle
        while(dut.io.idctrl.stateUint.peek.litValue != DecodeState.sIdle.litValue()) {
          dut.clock.step()
        }

      }
    }
  }



  it should "execute simple instructions and branch" in {
    simulationConfig()
    seed("Execute pipeline payload")
    val memfile = "src/resources/meminit/mem4.hex.txt"

    /* Instructions
    beq s0, s1, L1 //+4 (not taken)
    L1: pstart single
    estart
    add.vv vs2, vs1, vs0
    eend
    pend
    beq s0, s1, L1 //-20 (not taken)
    bne s0, s1, L2 //+24 (taken)
    pstart single
    estart
    sub.xx x3, x2, x0
    eend
    pend
    L2: istart single
    estart
    mul.ss s1, s2, s3
    eend
    pend
  */
    //Write to memory file
    val b0 = Array(BtypeInstruction(EQUAL, 0, 1, 4)).asInstanceOf[Array[Bundle with Instruction]]
    val p1 = wrapInstructions(Array(RtypeInstruction(2, 1, 0, ADD, RtypeMod.VV)))
    val b1 = Array(BtypeInstruction(EQUAL, 0, 1, -20)).asInstanceOf[Array[Bundle with Instruction]]
    val b2 = Array(BtypeInstruction(NEQ, 0, 1, 24)).asInstanceOf[Array[Bundle with Instruction]]
    val p2 = wrapInstructions(Array(RtypeInstruction(3, 2, 0, SUB, RtypeMod.XX)))
    val p3 = wrapInstructions(Array(RtypeInstruction(1, 2, 3, MUL, RtypeMod.SS)))

    val instrs = Array.concat(b0, p1, b1, b2, p2, p3)
    writeMemInitFile(memfile, instrs)
    //Execute
    test(new ExecutePipeline(memfile=memfile)) {dut =>
      testFun(dut)
    }
  }

  it should "use both threads" in {
    simulationConfig()
    seed("Execute pipeline both threads")
    val memfile = "src/resources/meminit/mem5.hex.txt"
    val program = "" +
      "pstart ndof\n" +
      "estart\n" +
      "abs.vv v0, v1, v2\n" +
      "eend\n" +
      "pend"
    val instrs = Assembler.assemble(program)
    writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)) {dut =>
      testFun(dut)
    }
  }

  it should "count to 5" in {
    simulationConfig()
    seed("Execute pipeline count to 5")
    val memfile = "src/resources/meminit/mem5.hex.txt"
    val program = "" +
    "pstart single\n" + //0
      "estart\n" + //4
      "add.is s1, s0, 5\n" + //8
      "add.ss s2, s0, s0\n" + //12
      "eend\n" + //16
      "pend\n" + //20
      "L1: \n" +
      "pstart single\n" + //24
      "estart\n" + //28
      "add.is s2, s2, 1\n" + //32
      "eend\n" + //36
      "pend\n" + //40
      "bne s2, s1, L1 //-20" //44
    val instrs = Assembler.assemble(program)
    writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)) {dut =>
      testFun(dut)
    }
  }

  it should "perform MAC instructions" in {
    simulationConfig()
    seed("Execute pipeline mac instructions")
    val memfile = "src/resources/meminit/mem5.hex.txt"
    /* Instructions
    pstart ndof
    estart
    sub.xv vs1, x0, vs3
    mac.sv s2, s1, vs2 //sum
    add.vv vs1, vs1, vs2
    eend
    iend
    pstart nelem
    estart
    mac.vv s0, vs1, vs2 //dot product
    eend
    iend
     */
    val p1 = wrapInstructions(Array(RtypeInstruction(1, 0, 3, SUB, RtypeMod.XV), RtypeInstruction(2, 1, 2, MAC, RtypeMod.SV), RtypeInstruction(1, 1, 2, ADD, RtypeMod.VV)), OtypeLen.NDOF)
    val p2 = wrapInstructions(Array(RtypeInstruction(0, 1, 2, MAC, RtypeMod.VV)), OtypeLen.NELEMVEC)
    val instrs = Array.concat(p1,p2)
    writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)) {dut =>
      testFun(dut)
    }
  }

  it should "perform a mac.kv instruction" in {
    simulationConfig()
    seed("Execute pipeline mac.kv instructions")
    val memfile = "src/resources/meminit/mem7.hex.txt"
    /*
    pstart single
    estart
    mac.kv v0, v1
    eend
    pend
     */
    val p1 = wrapInstructions(Array(RtypeInstruction(0, 1, 1, MAC, RtypeMod.KV)))
    writeMemInitFile(memfile, p1)
    test(new ExecutePipeline(memfile)) {dut =>
      testFun(dut)
    }
  }
}

class InstructionBuffer {
  var pstart: OtypeInstruction = new OtypeInstruction
  val load = ListBuffer.empty[StypeInstruction]
  val exec = ListBuffer.empty[RtypeInstruction]
  val store = ListBuffer.empty[StypeInstruction]
}

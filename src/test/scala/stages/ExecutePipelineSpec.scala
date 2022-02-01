package stages

import chisel3._
import chiseltest._
import common.{InstructionBuffer, calculateKVresult, fillInstructionBuffer}
import execution.BranchComp._
import execution.Opcode.{ADD, MAC, MUL, SUB}
import execution._

import utils.Assembler
import utils.Config._
import utils.Fixed._

import scala.collection.mutable.ListBuffer
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ExecutePipelineSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

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
    assert(math.abs(fixed2double((a.litValue-b.litValue).toLong)) < delta, s"[a=$a (${fixed2double(a)}), b=$b (${fixed2double(b)})]")
  }

  /**
   * Sets all global variables in the simulation
   * This includes references to the testers's [[sReg]], [[xReg]] and [[vReg]] variables
   * @param decode Decode stage of the DUT
   */
  def setGlobals(decode: Decode): Unit = {
    sReg = decode.sRegFile.arr
    xReg = Array(decode.threads(0).xRegFile.arr, decode.threads(1).xRegFile.arr)
    vReg = Array(decode.threads(0).vRegFile.arr, decode.threads(1).vRegFile.arr)
    execThread = 0
    memThread = 1
  }

    /**
     * Verifies that the branch outcome of the DUT matches the simulation's expected branch outcome
     * @param idctrl Interface between instruction decode and control stage
     * @param clock DUT clock
     * @param instr The branch instruction being evaluated
     */
    def verifyBranchOutcome(idctrl: IdControlIO, clock: Clock, instr: BtypeInstruction): Unit = {
      val a = sReg(instr.rs1.litValue.toInt)
      val b = sReg(instr.rs2.litValue.toInt)
      val comp = instr.comp
      val branch = branchOutcome(a, b, comp)
      clock.step() //Branch outcomes are evaluated on next clock cycle
      idctrl.branch.expect(branch.B)
      clock.step() //Another cc for next instruction be present
    }

  /**
   * Finalizes a sum-reduction operation by summing all values in MACresults, and storing the summed result in all elements of results
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
   * Expects the output of an instruction going into the vector register file
   * @param wbout Interface between execute writeback and decode stage
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectVREG(wbout: WbIdIO, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(instr.op.litValue == MAC.litValue && mod == RtypeMod.KV.litValue) {
      calculateKVresult(instr, results, wbout.rd.reg.peek(), vReg(execThread))
    } else if(mod == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, wbout.rd.reg.peek(), vReg(execThread))
    } else if (mod == RtypeMod.XV.litValue) {
      calculateXVresult(instr, results, wbout.rd.reg.peek(), xReg(execThread), vReg(execThread))
    } else if (mod == RtypeMod.SV.litValue) {
      calculateSVresult(instr, results, wbout.rd.reg.peek(), sReg, vReg(execThread))
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
    wbout.rd.rf.expect(RegisterFileType.VREG)
    assertEquals(wbout.wrData(0).peek(), results(0))
    for(i <- 0 until VREG_DEPTH) {
      results(i) = wbout.wrData(i).peek() //to avoid any incremental changes we store the calculated values
    }
  }


  /**
   * Expects the output of an instruction going into x-vector register file
   * @param wb The writeback port of the module
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectXREG(wb: WbIdIO, instr: RtypeInstruction, results: Array[SInt]): Unit = {
    if(instr.mod.litValue == RtypeMod.SX.litValue) {
      calculateSXresult(instr, results, sReg, xReg(execThread))
    } else if (instr.mod.litValue == RtypeMod.XX.litValue) {
      calculateXXresult(instr, results, xReg(execThread))
    } else if (instr.mod.litValue == RtypeMod.VV.litValue) {
      calculateVVresult(instr, results, 0.U, vReg(execThread)) //Setting rd == 0 since we know all of the expected output data
    } else {
      throw new IllegalArgumentException("Cannot decode modtype with result being xreg")
    }
    wb.rd.rf.expect(RegisterFileType.XREG)
    wb.rd.subvec.expect(0.U)
    assertEquals(wb.wrData(0).peek(), results(0))
    for (i <- 0 until NUM_PROCELEM) {
      results(i) = wb.wrData(i).peek()
    }
    wb.wrData(NUM_PROCELEM).expect(0.S)
  }

  /**
   * Expects the output of an instruction going into scalar register file
   * @param wbout The Writeback-Instruction Decode port of the module
   * @param results Result buffer
   * @param instr The instrution to check against
   */
  def expectSREG(wbout: WbIdIO, instr: RtypeInstruction, results: Array[SInt], MACresults: Array[SInt]): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.SS.litValue) {
      calculateSSresult(instr, results, sReg)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateReducedResult(results, MACresults)
    } else if(mod == RtypeMod.SV.litValue) {
      calculateReducedResult(results, MACresults)
    } else if(mod == RtypeMod.XX.litValue) {
      calculateRedXXresult(instr, results, xReg(execThread))
    } else {
      throw new IllegalArgumentException("R-type mod not recognized")
    }
    wbout.rd.rf.expect(RegisterFileType.SREG)
    wbout.rd.reg.expect(instr.rd)
    wbout.rd.subvec.expect(0.U)
    assertEquals(wbout.wrData(0).peek(), results(0))
    for(i <- 0 until NUM_PROCELEM) {
      results(i) = wbout.wrData(i).peek()
    }
    wbout.wrData(NUM_PROCELEM).expect(0.S)
  }

  /**
   * Checks whether the result returned by the writeback module matches our expectations.
   * Updates the register files in the DUT with all new values
   * @param wbout The output of the execute writeback stage
   * @param idout Output of the instruction decode stage
   * @param results Result buffer
   * @param MACresults Result buffer for MAC instructions
   * @param instr Instruction to expect the output of
   */
  def expectAndUpdate(wbout: WbIdIO, idout: IdExIO, clock: Clock, results: Array[SInt], MACresults: Array[SInt], instr: RtypeInstruction): Unit = {
    import RegisterFileType._
    val rf = getResultRegisterType(instr).litValue
    if(rf == VREG.litValue) {
      //KV, VV, XV and SV-instructions generate multiple results. We need to observe all of those results
      for(i <- 0 until VREG_SLOT_WIDTH) {
        while(!wbout.we.peek().litToBoolean) {
          //We still need to perform MAC instructions bookkeeping
          clock.step()
          handleMACSVandMACVV(idout, MACresults)
        }
        expectVREG(wbout, instr, results)
        updateVREG(instr, results, wbout.rd.reg.peek(), vReg(execThread))
        if (i < VREG_SLOT_WIDTH-1) {
          clock.step()
          handleMACSVandMACVV(idout, MACresults)
        } //Don't step after final result, this happens in testFun()
      }
    } else if (rf == XREG.litValue) {
      expectXREG(wbout, instr, results)
      updateXREG(instr, results, xReg(execThread))
    } else if (rf == SREG.litValue) {
      expectSREG(wbout, instr, results, MACresults)
      updateSREG(instr, results, sReg)
    } else {
      throw new IllegalArgumentException("Unknown register file type")
    }
  }

  /**
   * Performs bookkeeping when operating MAC instructions by storing the intermediate MAC results in a buffer
   * until they are used
   * @param idout Interface between Decode and Execute stages
   * @param MACresults The buffer storing MAC calculations until the result is presented
   */
  def handleMACSVandMACVV(idout: IdExIO, MACresults: Array[SInt]): Unit = {
    val isMACinstruction = idout.valid.peek().litToBoolean &&
      idout.opUInt.peek().litValue == Opcode.MAC.litValue &&
      idout.dest.rfUint.peek().litValue == RegisterFileType.SREG.litValue
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
        val a = idout.a(i).peek()
        val b = if(idout.useImm.peek().litToBoolean) idout.imm.peek() else idout.b(i).peek()
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
   * @param wbout Interface between execute writeback and decode stage
   * @param idout Interface between instruction decode and execute stages
   * @param clock DUT clock
   * @param iBuffer The instruction buffer being operated on
   */
  def performExecution(wbout: WbIdIO, idout: IdExIO, clock: Clock, iBuffer: InstructionBuffer): Unit = {
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
    } else if (iBuffer.pstart.len.litValue == NELEMSTEP.litValue) {
      maxProgress = NELEMLENGTH
      progressIncr = 1
      MAClength = 1
    }

    //We need a special check to ensure correct functionality if only instruction is of type mac.vv/mac.sv
    if(iBuffer.exec.length == 1 && iBuffer.exec(0).op.litValue == MAC.litValue &&
      (iBuffer.exec(0).mod.litValue == RtypeMod.SV.litValue || iBuffer.exec(0).mod.litValue == RtypeMod.VV.litValue)) {
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
        handleMACSVandMACVV(idout, MACresults)

        //When things are presented on writeback output, write back into register file
        if(wbout.we.peek().litToBoolean) {
          //MAC.VV, MAC.SV instructions only output on the final cycle of that packet. Skip them while working towards the final outputs
          if(iBuffer.exec(instrCnt).op.litValue == MAC.litValue && getResultRegisterType(iBuffer.exec(instrCnt)).litValue == RegisterFileType.SREG.litValue && progress != (maxProgress-progressIncr)) {
            instrCnt += 1
          }
          expectAndUpdate(wbout, idout, clock, results, MACresults, iBuffer.exec(instrCnt))
          instrCnt += 1
        }
        clock.step()
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
//
//  /**
//   * Fills the instruction buffer with all instructions in the current instruction packet
//   * @param idctrl Interface between instruction decode and control stages
//   * @param clock DUT clock
//   * @param iBuffer The instruction buffer to be filled
//   */
//  def fillInstructionBuffer(idctrl: IdControlIO, clock: Clock, iBuffer: InstructionBuffer): Unit = {
//    var i = 1
//    var instr: UInt = 0.U //Default value
//    var fmt: InstructionFMT.Type = InstructionFMT.RTYPE //Default value
//
//    iBuffer.load.clear()
//    iBuffer.exec.clear()
//    iBuffer.store.clear()
//    do {
//      instr = idctrl.instr.peek()
//      fmt = InstructionFMT(instr(7, 6).litValue.toInt)
//      if (fmt.litValue == InstructionFMT.OTYPE.litValue) {
//        //Do nothing
//      } else if (fmt.litValue == InstructionFMT.RTYPE.litValue) {
//        iBuffer.exec += RtypeInstruction(instr)
//      } else if (fmt.litValue == InstructionFMT.STYPE.litValue) {
//        iBuffer.store += StypeInstruction(instr)
//      } else {
//        throw new IllegalArgumentException("Unable to decode format")
//      }
//      i += 1
//      clock.step()
//      //Continue until we get iend instruction
//    } while(instr.litValue != OtypeInstruction(OtypeSE.END, OtypeMod.PACKET).toUInt().litValue)
//  }

  /**
   * Main testing function for the execute pipeline tester. Performs global variables setup
   * @param dut the DUT
   */
  def testFun(dut: ExecutePipeline): Unit = {
    setGlobals(dut.decode)
    dut.clock.step() //1 cycle to get instruction into decode stage
    assert(dut.io.idctrl.instr.peek().litValue != 0, "Peeked instruction with value 0, did not init memory correctly")
    while (dut.io.idctrl.instr.peek().litValue != 0) {
      //Always snoop on instruction at decode stage
      val instr = dut.io.idctrl.instr.peek()
      val fmt = InstructionFMT(instr(7, 6).litValue.toInt)

      if (fmt.litValue == InstructionFMT.BTYPE.litValue) {
        verifyBranchOutcome(dut.io.idctrl, dut.clock, BtypeInstruction(instr))
      } else if (fmt.litValue == InstructionFMT.OTYPE.litValue) { //Executable packet
        val iBuffer = new InstructionBuffer
        iBuffer.pstart = OtypeInstruction(instr)
        fillInstructionBuffer(dut.io.idctrl, dut.clock, iBuffer)
        performExecution(dut.io.wbout, dut.io.idout, dut.clock, iBuffer)
        //Wait until all threads are idle
        while (dut.io.idctrl.stateUint.peek().litValue != DecodeState.sIdle.litValue) {
          dut.clock.step()
        }
      }
    }
  }

  it should "execute simple instructions and branch" in {
    simulationConfig()
    seed("Execute pipeline payload")
    val memfile = "src/resources/meminit/mem4.hex.txt"

    val program = "beq s0, s1, L1\n" +
      "L1:\n" +
      "pstart single\n" +
      "estart\n" +
      "add.vv v2, v1, v0\n" +
      "eend\n" +
      "pend\n" +
      "beq s0, s1, L1\n" +
      "bne s0, s1, L2\n" +
      "pstart single\n" +
      "estart\n" +
      "sub.xx x3, x2, x0\n" +
      "eend\n" +
      "pend\n" +
      "L2:\n" +
      "pstart single\n" +
      "estart\n" +
      "mul.ss s1, s2, s3\n" +
      "eend\n" +
      "pend"
    val instrs = Assembler.assemble(program)
    Assembler.writeMemInitFile(memfile, instrs)
    //Execute
    test(new ExecutePipeline(memfile=memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      testFun(dut)
    }
  }

  it should "use both threads" in {
    simulationConfig()
    seed("Execute pipeline both threads")
    val memfile = "src/resources/meminit/mem5.hex.txt"
    val program = "" +
      "pstart nelemvec\n" +
      "estart\n" +
      "abs.vv v0, v1, v2\n" +
      "eend\n" +
      "pend"
    val instrs = Assembler.assemble(program)
    Assembler.writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
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
      "add.is s3, s0, 5\n" + //8
      "add.ss s2, s0, s0\n" + //12
      "eend\n" + //16
      "pend\n" + //20
      "L1: \n" +
      "pstart single\n" + //24
      "estart\n" + //28
      "add.is s2, s2, 1\n" + //32
      "eend\n" + //36
      "pend\n" + //40
      "bne s2, s3, L1 " //44
    val instrs = Assembler.assemble(program)
    Assembler.writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.setTimeout(150)
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
    sub.xv v1, x0, v3
    mac.sv s2, s1, v2 //sum
    add.vv v1, v1, v2
    eend
    iendv
    pstart nelem
    estart
    mac.vv s0, v1, v2 //dot product
    eend
    iend
     */
    val p1 = wrapInstructions(Array(RtypeInstruction(1, 0, 3, SUB, RtypeMod.XV), RtypeInstruction(2, 1, 2, MAC, RtypeMod.SV), RtypeInstruction(1, 1, 2, ADD, RtypeMod.VV)), OtypeLen.NELEMVEC)
    val p2 = wrapInstructions(Array(RtypeInstruction(0, 1, 2, MAC, RtypeMod.VV)), OtypeLen.NELEMVEC)
    val instrs = Array.concat(p1,p2)
    writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile=memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
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

  it should "perform density filtering" in {
    simulationConfig()
    seed("Execute pipeline density filter", Some(6620151348395930099L))
    val memfile = "src/resources/meminit/densityfilter.txt"

    val program = "pstart nelemstep \n" + //Must use nelemstep as packet length as we're executing red.xx instructions
      "estart\n" +
      "div.xx x2, x2, x2\n" +
      "div.xx x3, x3, x3\n" +
      "div.xx x4, x4, x4\n" +
      "mul.ix x2, x2, 0.5\n" +
      "mul.ix x3, x3, 0.08578\n" +
      "mul.ix x4, x4, 0.08578\n" +
      "red.xx s1, x0, x2\n" +
      "red.xx s2, x0, x3\n" +
      "red.xx s3, x0, x4\n" +
      "add.ss s1, s1, s2\n" +
      "add.ss s1, s1, s3\n" +
      "add.is s1, s1, 1.5\n" +
      "div.is s1, s1, 1\n" +
      "mul.sx x1, s1, x1\n" +
      "eend\n" +
      "pend"

    val instrs = Assembler.assemble(program)
    Assembler.writeMemInitFile(memfile, instrs)
    test(new ExecutePipeline(memfile)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      dut.clock.setTimeout(200)
      testFun(dut)
    }
  }
}


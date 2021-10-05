import chiseltest._
import chisel3._
import execution.Opcode.{MAC, RED}
import execution._
import utils.Config._
import utils.Fixed._
import org.scalatest.{FlatSpec, Matchers}

import scala.collection.mutable.ListBuffer

/**
 * A helper class containing a number of commonly used functions shared across multiple simulations
 */
package object common extends FlatSpec with Matchers { //Must extend flatspec & matchers to have assertEquals defined here
  /**
   * Verifies that the branch outcome of the DUT matches the simulation's expected branch outcome
   * @param dut The DUT
   * @param instr The branch instruction being evaluated
   */
  def verifyBranchOutcome(idctrl: IdControlIO, clock: Clock, instr: BtypeInstruction, mc: SimulationContainer): Unit = {
    val a = mc.sReg(instr.rs1.litValue.toInt)
    val b = mc.sReg(instr.rs2.litValue.toInt)
    val comp = instr.comp
    val branch = branchOutcome(a, b, comp)
    clock.step() //Branch outcomes are evaluated on next clock cycle
    idctrl.branch.expect(branch.B)
    clock.step() //Another cc for next instruction be present
  }

  /**
   * Verifies that two fixed-point numbers represent the same value, to within some margin of error
   * @param a The first value
   * @param b The second value
   * @param delta Maximum allowed deviation (not inclusive)
   */
  def assertEquals(a: SInt, b: SInt, delta: Double = 0.01): Unit = {
    org.scalatest.Assertions.assert(math.abs(fixed2double((a.litValue-b.litValue).toLong)) < delta, s"[a=$a (${fixed2double(a)}), b=$b (${fixed2double(b)})]")
  }

  /**
   * A container class used to group the instructions in an instruction packet,
   * making it easier to simulate that instruction packet
   */
  class InstructionBuffer {
    var pstart: OtypeInstruction = new OtypeInstruction
    val load = ListBuffer.empty[StypeInstruction]
    val exec = ListBuffer.empty[RtypeInstruction]
    val store = ListBuffer.empty[StypeInstruction]
  }

  /**
   * Fills the instruction buffer with all instructions in the current instruction packet
   * @param idctrl Interface between instruction decode and control stages
   * @param clock DUT clock
   * @param iBuffer The instruction buffer to be filled
   */
  def fillInstructionBuffer(idctrl: IdControlIO, clock: Clock, iBuffer: InstructionBuffer): Unit = {
    var i = 1
    var instr: UInt = 0.U //Default value
    var fmt: InstructionFMT.Type = InstructionFMT.RTYPE //Default value

    iBuffer.load.clear()
    iBuffer.exec.clear()
    iBuffer.store.clear()
    iBuffer.pstart = OtypeInstruction(idctrl.instr.peek())
    do {
      instr = idctrl.instr.peek
      fmt = InstructionFMT(instr(7, 6).litValue.toInt)
      if (fmt.litValue == InstructionFMT.OTYPE.litValue) {
//        val o = OtypeInstruction(idctrl.instr.peek())
//        if(o.mod.litValue() == OtypeMod.PACKET.litValue() && o.se.litValue() == OtypeSE.START.litValue()) {
//          iBuffer.pstart = o
//        }
      } else if (fmt.litValue == InstructionFMT.RTYPE.litValue()) {
        iBuffer.exec += RtypeInstruction(instr)
      } else if (fmt.litValue == InstructionFMT.STYPE.litValue()) {
        val s = StypeInstruction(instr)
        if(s.ls.litValue == StypeLoadStore.LOAD.litValue()) {
          iBuffer.load += s
        } else {
          iBuffer.store += s
        }
      } else {
        throw new IllegalArgumentException("Unable to decode format")
      }
      i += 1
      clock.step()
      //Continue until we get pend instruction
    } while(instr.litValue != OtypeInstruction(OtypeSE.END, OtypeMod.PACKET).toUInt().litValue())
  }


  /**
   * Performs bookkeeping when operating MAC instructions by storing the intermediate MAC results in a buffer
   * until they are used. RED instructions are not handled here
   * @param idex Interface between ID and EX stages
   * @param sc The Simulation Container used for this simulation
   */
  def handleMACSVandMACVV(idex: IdExIO, sc: SimulationContainer): Unit = {
    val isMACinstruction = idex.valid.peek().litToBoolean &&
      idex.opUInt.peek.litValue == Opcode.MAC.litValue() &&
      idex.dest.rfUint.peek.litValue == RegisterFileType.SREG.litValue
    val macLimit = ELEMS_PER_VSLOT / NUM_PROCELEM //Number of elements added to each result register on each MAC iteration
    /*
      When a mac is noticed on the output, the actual values arrive one clock cycle later since the reg file is a syncreadmem
      To support mac instructions, we first notice the mac instruction and set firstMac false
      On subsequent iterations, the first statement (isMacInstruction && !firstMac) will cause the update logic to be executed
      On the final iteration, isMacInstruction will be low (since control signals are ahead by one clock cycle).
      To solve this, we count up, to ensure that the correct number of values have been added together. Once macCnt == macLimit, we reset the mac update logic
     */
    if((isMACinstruction && !sc.firstMAC) || (0 < sc.macCnt && sc.macCnt < macLimit)) {
      for(i <- 0 until NUM_PROCELEM) {
        val a = idex.a(i).peek
        val b = if(idex.useImm.peek.litToBoolean) idex.imm.peek else idex.b(i).peek
        sc.MACresults(i) = fixedAdd(sc.MACresults(i), fixedMul(a,b))
      }
      sc.macCnt += 1
    } else if (isMACinstruction && sc.firstMAC) {
      sc.firstMAC = false
    } else if (!isMACinstruction && sc.macCnt >= macLimit) {
      sc.firstMAC = true
      sc.macCnt = 0
    }
  }


  /**
   * Checks whether the result returned by the writeback module matches our expectations.
   * Updates the register files in the DUT with all new values
   * @param wbid The output of the execute writeback stage
   * @param idex Output of the instruction decode stage
   * @param sc The simulation container used for this simulation
   * @param instr Instruction to expect the output of
   */
  def expectAndUpdate(wbid: WbIdIO, idex: IdExIO, clock: Clock, sc: SimulationContainer, instr: RtypeInstruction): Unit = {
    import RegisterFileType._
    val rf = getResultRegisterType(instr).litValue
    if(rf == VREG.litValue) {
      //KV, VV, XV and SV-instructions generate multiple results. We need to observe all of those results
      for(i <- 0 until VREG_SLOT_WIDTH) {
        while(!wbid.we.peek.litToBoolean) {
          //We still need to perform MAC instructions bookkeeping
          clock.step()
          handleMACSVandMACVV(idex, sc)
        }
        expectVREG(wbid, instr, sc)
        updateVREG(instr, sc.results, wbid.rd.reg.peek, sc.vReg(sc.execThread))
        if (i < VREG_SLOT_WIDTH-1) {
          clock.step()
          handleMACSVandMACVV(idex, sc)
        } //Don't step after final result, this happens in testFun()
      }
    } else if (rf == XREG.litValue) {
      expectXREG(wbid, instr, sc)
      updateXREG(instr, sc.results, sc.xReg(sc.execThread))
    } else if (rf == SREG.litValue) {
      expectSREG(wbid, instr, sc)
      updateSREG(instr, sc.results, sc.sReg)
    } else {
      throw new IllegalArgumentException("Unknown register file type")
    }
  }

  /**
   * Takes an index and checks whether this index corresponds to a fixed degree of freedom.
   * If the index is a fixed degree, returns that index. Otherwise returns -1
   * @param index An index corresponding to one of the DOFs in the grid
   * @return The input value if that index corresponds to a fixed DOF, -1 otherwise
   */
  def manipulateFixedDof(index: Int): Int = {
    //Fixed dofs are the lower dofs in layer x=0. Only DOFs in colouring 0,1,2,3 can be fixed
    //To this end, index must be in specified range AND be in colourings 0,1,2,3 => bit 2 must not be set
    if((0 until NY*NZ*3 contains index) && (index & 4) == 0) index else -1 //NELY*NELZ*3 is all indices where x=0
  }


  /**
   * Verifies that the branch outcome of the DUT matches the simulation's expected branch outcome
   * @param idctrl Interface between instruction decode and control stage
   * @param clock DUT clock
   * @param instr The branch instruction being evaluated
   * @param sReg Handle to the s-register file in simulation
   */
  def verifyBranchOutcome(idctrl: IdControlIO, clock: Clock, instr: BtypeInstruction, sReg: Array[SInt]): Unit = {
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
   * @param wbid Interface between execute writeback and decode stage
   * @param instr The instrution to check against
   * @param sc The simulation container being used
   */
  def expectVREG(wbid: WbIdIO, instr: RtypeInstruction, sc: SimulationContainer): Unit = {
    val mod = instr.mod.litValue
    if(instr.op.litValue() == MAC.litValue && mod == RtypeMod.KV.litValue) {
      calculateKVresult(instr, sc.results, wbid.rd.reg.peek, sc.vReg(sc.execThread))
    } else if(mod == RtypeMod.VV.litValue) {
      calculateVVresult(instr, sc.results, wbid.rd.reg.peek, sc.vReg(sc.execThread))
    } else if (mod == RtypeMod.XV.litValue) {
      calculateXVresult(instr, sc.results, wbid.rd.reg.peek, sc.xReg(sc.execThread), sc.vReg(sc.execThread))
    } else if (mod == RtypeMod.SV.litValue) {
      calculateSVresult(instr, sc.results, wbid.rd.reg.peek, sc.sReg, sc.vReg(sc.execThread))
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
    wbid.rd.rf.expect(RegisterFileType.VREG)
    assertEquals(wbid.wrData(0).peek, sc.results(0))
    for(i <- 0 until VREG_DEPTH) {
      sc.results(i) = wbid.wrData(i).peek //to avoid any incremental changes we store the calculated values
    }
  }


  /**
   * Expects the output of an instruction going into x-vector register file
   * @param wbid Interface between execute writeback and decode stage
   * @param instr The instrution to check against
   * @param sc The simulation container being used
   */
  def expectXREG(wbid: WbIdIO, instr: RtypeInstruction, sc: SimulationContainer): Unit = {
    if(instr.mod.litValue == RtypeMod.SX.litValue) {
      calculateSXresult(instr, sc.results, sc.sReg, sc.xReg(sc.execThread))
    } else if (instr.mod.litValue == RtypeMod.XX.litValue) {
      calculateXXresult(instr, sc.results, sc.xReg(sc.execThread))
    } else if (instr.mod.litValue == RtypeMod.VV.litValue) {
      calculateVVresult(instr, sc.results, 0.U, sc.vReg(sc.execThread)) //Setting rd == 0 since we know all of the expected output data
    } else {
      throw new IllegalArgumentException("Cannot decode modtype with result being xreg")
    }
    wbid.rd.rf.expect(RegisterFileType.XREG)
    wbid.rd.subvec.expect(0.U)
    assertEquals(wbid.wrData(0).peek, sc.results(0))
    for (i <- 0 until NUM_PROCELEM) {
      sc.results(i) = wbid.wrData(i).peek
    }
    wbid.wrData(NUM_PROCELEM).expect(0.S)
  }

  /**
   * Expects the output of an instruction going into scalar register file
   * @param wbid Interface between execute writeback and decode stage
   * @param instr The instrution to check against
   * @param sc The simulation container being used
   */
  def expectSREG(wbid: WbIdIO, instr: RtypeInstruction, sc: SimulationContainer): Unit = {
    val mod = instr.mod.litValue
    if(mod == RtypeMod.SS.litValue) {
      calculateSSresult(instr, sc.results, sc.sReg)
    } else if(mod == RtypeMod.VV.litValue) {
      calculateReducedResult(sc.results, sc.MACresults)
    } else if(mod == RtypeMod.SV.litValue) {
      calculateReducedResult(sc.results, sc.MACresults)
    } else if(mod == RtypeMod.XX.litValue) {
      calculateRedXXresult(instr, sc.results, sc.xReg(sc.execThread))
    } else {
      throw new IllegalArgumentException("R-type mod not recognized")
    }
    wbid.rd.rf.expect(RegisterFileType.SREG)
    wbid.rd.reg.expect(instr.rd)
    wbid.rd.subvec.expect(0.U)
    assertEquals(wbid.wrData(0).peek, sc.results(0))
    for(i <- 0 until NUM_PROCELEM) {
      sc.results(i) = wbid.wrData(i).peek
    }
    wbid.wrData(NUM_PROCELEM).expect(0.S)
  }


  /**
   * Calculates the result of an instruction with R-type modifier KV
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateKVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        vReg: Array[Array[SInt]]): Unit = {
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
        val b = vReg(rs1*VREG_SLOT_WIDTH + rdOffset)(j)
        results(i) = fixedAdd(results(i), fixedMul(a, b))
      }
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SS
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSSresult(instr: RtypeInstruction, results: Array[SInt],
                        sReg: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = sReg(rs2)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSXresult(instr: RtypeInstruction, results: Array[SInt],
                        sReg: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = xReg(rs2)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateXXresult(instr: RtypeInstruction, results: Array[SInt],
                        xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else xReg(rs1)(i)
      val b = xReg(rs2)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SV. Does not calculate summations (mac.sv).
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateSVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        sReg: Array[SInt], vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)
    for(i <- 0 until VREG_DEPTH) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }
  /**
   * Calculates the result of an instruction with R-type modifier VV. Does not calculate dot products (mac.vv)
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateVVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)

    if(instr.op.litValue == RED.litValue) { //RED.VV instructions require special care
      //Multiply all VREG_DEPTH values in each register slot, and them sum them together. Place into result register
      for(s <- 0 until VREG_SLOT_WIDTH) {
        results(s) = 0.S(FIXED_WIDTH.W)
        for(e <- 0 until VREG_DEPTH) {
          val a = vReg(rs1*VREG_SLOT_WIDTH+s)(e)
          val b = vReg(rs2*VREG_SLOT_WIDTH+s)(e)
          results(s) = fixedAdd(results(s), fixedMul(a,b))
        }
      }
    } else {
      for(i <- 0 until VREG_DEPTH) {
        val a = if(instr.immflag.litToBoolean) imm else vReg(rs1*VREG_SLOT_WIDTH+rdOffset)(i)
        val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
        results(i) = calculateRes(instr, a, b)
      }
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XV
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateXVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        xReg: Array[Array[SInt]], vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)
    val a = if(instr.immflag.litToBoolean) imm else xReg(rs1)(rdOffset)
    for(i <- 0 until VREG_DEPTH) {
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of a red.xx instruction
   * @param instr The instruction
   * @param results The results registers
   */
  def calculateRedXXresult(instr: RtypeInstruction, results: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for(i <- results.indices) {
      results(i) = 0.S(FIXED_WIDTH.W)
    }
    for(i <- 0 until XREG_DEPTH) {
      val a = xReg(rs1)(i)
      val b = xReg(rs2)(i)
      results(0) = fixedAdd(results(0), fixedMul(a,b))
    }
  }


  /**
   * Updates the simulation vector register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   * @param rdDUT Destination register, as peeked from the DUT
   */
  def updateVREG(instr: RtypeInstruction, results: Array[SInt], rdDUT: UInt, vReg: Array[Array[SInt]]): Unit = {
    val rd = instr.rd.litValue.toInt
    //    val rdOffset = dut.io.wb.rd.reg.peek.litValue.toInt % VREG_SLOT_WIDTH
    val rdOffset = rdDUT.litValue.toInt % VREG_SLOT_WIDTH
    for (j <- 0 until VREG_DEPTH) {
      vReg(rd * VREG_SLOT_WIDTH + rdOffset)(j) = results(j)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateXREG(instr: RtypeInstruction, results: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rd = instr.rd.litValue.toInt
    for(i <- 0 until NUM_PROCELEM) {
      xReg(rd)(i) = results(i)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateSREG(instr: RtypeInstruction, results: Array[SInt], sReg: Array[SInt]): Unit = {
    val rd = instr.rd.litValue.toInt
    if(rd != 0) { sReg(rd) = results(0) }
  }

}

import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}
import chisel3._
import chisel3.util.log2Ceil
import common._
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import execution.Opcode.MAC
import execution._
import firrtl.AnnotationSeq
import memory.{AddressDecode, elementIndex, genIJKmultiple, getEdn1Indices, getEdn2Indices, getEdof, getFcnIndices, getFdof}
import utils.Config._
import utils.{Assembler, SynthesisMemInit}
import utils.Fixed._

import scala.io.Source

class TopLevelStageSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Top Level"

  /**
   * Determines the expected data when performing a memory load operation by retrieveing the corresponding entries
   * in the simulation's memory.
   * @param indices The indices which should be accessed. If an index is less than 0, it is interpreted as a non-valid index and 0 is returned
   * @param baseAddr The base address for the memory load operation
   * @param sc The simulation container object being used for this test
   * @return A sequence of read data such that ret(0) is the data stored at mem(baseAddr+indices(0)), etc
   */
  def determineExpectedLoadData(indices: Seq[Int], baseAddr: StypeBaseAddress.Type, sc: SimulationContainer): Seq[SInt] = {
    val baseAddrDec = AddressDecode.mapping(baseAddr.litValue.toInt)
    val addresses = indices.map(_ + baseAddrDec)
    val maxProg = maxProgress(sc.iBuffer)

    //Get from memory
    val rdData = Array.ofDim[SInt](indices.length)
    for(i <- addresses.indices) {
      if((0 until maxProg) contains indices(i)) { //Only load actual value if index is valid
        val bank = addresses(i) % NUM_MEMORY_BANKS
        val index = addresses(i) >> log2Ceil(NUM_MEMORY_BANKS)
        rdData(i) = sc.mem(bank)(index)
      } else {
        rdData(i) = 0.S
      }
    }
    rdData
  }

  /**
   * Stores data into simulation memory
   * @param indices The indices to be accessed
   * @param baseAddr The base address for the memory store operation
   * @param sc The simulation container object being used
   * @param wrData The data to be written
   */
  def storeData(indices: Seq[Int], clock: Clock, baseAddr: StypeBaseAddress.Type, sc: SimulationContainer, wrData: Seq[SInt]): Unit = {
    val maxProg = maxProgress(sc.iBuffer)
    val baseAddrDec = AddressDecode.mapping(baseAddr.litValue.toInt)
    val addresses = indices.map(_ + baseAddrDec)

    for(i <- addresses.indices) {
      if((0 until maxProg) contains indices(i)) { //Only perform store operations on valid indices
        val bank = addresses(i) % NUM_MEMORY_BANKS
        val index = addresses(i) >> log2Ceil(NUM_MEMORY_BANKS)
        sc.mem(bank)(index) = wrData(i)
      }
    }
    clock.step()
  }

  /**
   * Performs the actual data loading into a VREG
   * @param memid Interface between memory writeback and instruction decode stages
   * @param clock DUT clock
   * @param rd Destination V-register
   * @param expected Expected data. Each entry of expected(i) must contain exactly 24 entries
   * @param sc The simulation container used for this simulation
   */
  def loadIntoVreg(memid: WbIdIO, clock: Clock, rd: UInt, expected: Seq[Seq[SInt]], sc: SimulationContainer): Unit = {
    for(i <- expected.indices) {
      while(!memid.we.peek.litToBoolean) { clock.step() }

      val exp = expected(i)
      val reg = rd.litValue.toInt*VREG_SLOT_WIDTH + i
      for(j <- exp.indices) {
        val rdData = memid.wrData(j).peek()
        assertEquals(rdData, exp(j))
        sc.vReg(sc.memThread)(reg)(j) = rdData
      }
      memid.rd.rf.expect(RegisterFileType.VREG)
      memid.rd.reg.expect(reg.U)
      clock.step()
    }
  }

  /**
   * Loads data from memory into an x-register in the thread which has memory access
   * @param memid Interface between memory writeback and instruction decode stages
   * @param clock DUT clock
   * @param rd The destination register encoded in the instruction
   * @param expected Expected data. Must be at most 8 entries large
   * @param sc The simulation container used for this simulation
   */
  def loadIntoXreg(memid: WbIdIO, clock: Clock, rd: UInt, expected: Seq[SInt], sc: SimulationContainer): Unit = {
    while(!memid.we.peek.litToBoolean) { clock.step() }

    val reg = rd.litValue.toInt
    for(j <- expected.indices) {
      val rdData = memid.wrData(j).peek()
      assertEquals(rdData, expected(j))
      sc.xReg(sc.memThread)(reg)(j) = rdData
    }
    memid.rd.rf.expect(RegisterFileType.XREG)
    memid.rd.reg.expect(rd)
    clock.step()
  }

  /**
   * Value of maxProgress determines when memory load/stores are invalidated
   * @param iBuffer The instruction buffer in use
   * @return
   */
  def maxProgress(iBuffer: InstructionBuffer): Int = if(iBuffer.pstart.len.litValue == OtypeLen.NELEMVEC.litValue) NELEMSIZE else NDOFSIZE


  /**
   * Performs the logic necessary to execute a ld.edn1 instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadEdn1(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = sc.ijkBase(sc.memThread)
    val indices = getEdn1Indices(ijk)
    val expectedData = determineExpectedLoadData(indices, instr.baseAddr, sc)
    loadIntoXreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a ld.edn2 instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadEdn2(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = sc.ijkBase(sc.memThread)
    val indices = getEdn2Indices(ijk)
    val expectedData = determineExpectedLoadData(indices, instr.baseAddr, sc)
    loadIntoXreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a ld.fcn instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadFcn(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = sc.ijkBase(sc.memThread)
    val indices = getFcnIndices(ijk)
    val expectedData = determineExpectedLoadData(indices, instr.baseAddr, sc)
    loadIntoXreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a ld.sel instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadSel(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = sc.ijkBase(sc.memThread)
    val indices = Seq(elementIndex(ijk))
    val expectedData = determineExpectedLoadData(indices, instr.baseAddr, sc)
    loadIntoXreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a st.sel instruction
   * @param idmem Interface between instruction decode and memory stage
   * @param clock DUT clock
   * @param sc Simulation container object
   * @param instr Current S-type instruction
   */
  def performStoreSel(idmem: IdMemIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = sc.ijkBase(sc.memThread)
    val indices = Seq(elementIndex(ijk))

    while(!idmem.writeQueue.valid.peek().litToBoolean) { clock.step() }
    val wrData: SInt = idmem.writeQueue.bits.wrData(0).peek
    storeData(indices, clock, instr.baseAddr, sc, Seq(wrData))
  }

  /**
   * Performs the logic necessary to execute a ld.elem instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadElem(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = genIJKmultiple(start=Some(sc.ijkBase(sc.memThread))) //All IJK tuples accessed
    val indices = ijk.map(elementIndex)
    val expectedData = determineExpectedLoadData(indices, instr.baseAddr, sc)
    loadIntoXreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a st.elem instruction
   * @param idmem Interface between instruction decode and memory stage
   * @param clock DUT clock
   * @param sc Simulation container object
   * @param instr Current S-type instruction
   */
  def performStoreElem(idmem: IdMemIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = genIJKmultiple(start=Some(sc.ijkBase(sc.memThread))) //Elements accessed
    val indices = ijk.map(elementIndex) //Indices accessed
    val wrData = Array.ofDim[SInt](NUM_MEMORY_BANKS)

    for(ind <- indices) {
      while(!idmem.writeQueue.valid.peek().litToBoolean) { clock.step() }
      val wrData = idmem.writeQueue.bits.wrData(0).peek
      storeData(Seq(ind), clock, instr.baseAddr, sc, Seq(wrData))
    }
  }
  /**
   * Performs the logic necessary to execute a ld.dof instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadDof(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = genIJKmultiple(start=Some(sc.ijkBase(sc.memThread))) //Generate all ijk tuples accessed
    val edof = ijk.map(e => getEdof(e(0), e(1), e(2))) //And the corresponding memory locations
    val expectedData = edof.map(e => determineExpectedLoadData(e, instr.baseAddr, sc))
    loadIntoVreg(memid, clock, instr.rsrd, expectedData, sc)
  }

  /**
   * Performs the logic necessary to execute a st.dof instruction
   * @param idmem Interface between instruction decode and memory stage
   * @param clock DUT clock
   * @param sc Simulation container object
   * @param instr Current S-type instruction
   */
  def performStoreDof(idmem: IdMemIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = genIJKmultiple(start=Some(sc.ijkBase(sc.memThread))) //Generate accessed ijk-tuples
    val indices = ijk.flatMap(e => getEdof(e(0), e(1), e(2))).grouped(NUM_MEMORY_BANKS) //Generate dof values and split into sections of 8
    val wrData = Array.ofDim[SInt](NUM_MEMORY_BANKS)

    for(ind <- indices) {
      while(!idmem.writeQueue.valid.peek.litToBoolean) { clock.step() }
      for(i <- wrData.indices) {
        wrData(i) = idmem.writeQueue.bits.wrData(i).peek
      }
      storeData(ind, clock, instr.baseAddr, sc, wrData)
    }
  }

  /**
   * Performs the logic necessary to execute a st.fdof instruction
   * @param idmem Interface between instruction decode and memory stage
   * @param clock DUT clock
   * @param sc Simulation container object
   * @param instr Current S-type instruction
   */
  def performStoreFdof(idmem: IdMemIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val ijk = genIJKmultiple(start=Some(sc.ijkBase(sc.memThread)))

    val indices = ijk.flatMap(e => getFdof(e(0), e(1), e(2))).grouped(NUM_MEMORY_BANKS)
    val wrData = Array.ofDim[SInt](NUM_MEMORY_BANKS)
    for(ind <- indices) {
      while(!idmem.writeQueue.valid.peek.litToBoolean) { clock.step() }
      for(i <- wrData.indices) {
        wrData(i) = idmem.writeQueue.bits.wrData(i).peek
      }
      storeData(ind, clock, instr.baseAddr, sc, wrData)
    }
  }



  /**
   * Performs the logic necessary to execute a ld.vec instruction
   * @param memid Interface between memory writeback and instruction decode stage
   * @param clock DUT clock
   * @param sc Simulation container object with test-global values
   * @param instr The current S-type instruction
   */
  def performLoadVec(memid: WbIdIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val baseIndex = sc.vecBaseIndex(sc.memThread)
    val indices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(baseIndex+n*VREG_DEPTH, baseIndex+(n+1)*VREG_DEPTH))
    val expectedData = indices.map(i => determineExpectedLoadData(i, instr.baseAddr, sc))

    loadIntoVreg(memid, clock, instr.rsrd, expectedData, sc)
  }


  /**
   * Performs the logic necessary to execute a st.vec instruction
   * @param idmem Interface between instruction decode and memory stage
   * @param clock DUT clock
   * @param sc Simulation container object
   * @param instr Current S-type instruction
   */
  def performStoreVec(idmem: IdMemIO, clock: Clock, sc: SimulationContainer, instr: StypeInstruction): Unit = {
    val baseIndex = sc.vecBaseIndex(sc.memThread) //Base index to access
    val indices = Seq.tabulate(VREG_DEPTH)(n => Seq.range(baseIndex+n*NUM_MEMORY_BANKS, baseIndex+(n+1)*NUM_MEMORY_BANKS)) //All indices to access
    val wrData = Array.ofDim[SInt](NUM_MEMORY_BANKS)
    for(ind <- indices) { //For each set of indices, get data and store it
      while(!idmem.writeQueue.valid.peek.litToBoolean) { clock.step() }
      for (i <- wrData.indices) {
        wrData(i) = idmem.writeQueue.bits.wrData(i).peek
      }
      storeData(ind, clock, instr.baseAddr, sc, wrData)
    }
  }


  /**
   * Wrapper function used to encapsulate all operations happening in the memory store part of an instruction packet
   * @param memid Interface between memory and instruction decode
   * @param clock DUT clock
   * @param sc Simulation container object
   */
  def handleMemoryLoadOperations(memid: WbIdIO, clock: Clock, sc: SimulationContainer): Unit = {
    for(li <- sc.iBuffer.load) {
      while(!memid.we.peek.litToBoolean) {
        clock.step()
      }

      println(f"${sc.memThread} Loading instruction $li")
      //Given type of load instruction, expect load data (using determineExpectedLoadData) and performExpectLoad
      //then, update register files.
      //Must know current iteration to determine currently used ijk/vec values
      if(li.mod.litValue == StypeMod.VEC.litValue) {
        performLoadVec(memid, clock, sc, li)
      } else if (li.mod.litValue == StypeMod.DOF.litValue) {
        performLoadDof(memid, clock, sc, li)
      } else if (li.mod.litValue == StypeMod.ELEM.litValue) {
        performLoadElem(memid, clock, sc, li)
      } else if (li.mod.litValue == StypeMod.EDN1.litValue) {
        performLoadEdn1(memid, clock, sc, li)
      } else if (li.mod.litValue == StypeMod.EDN2.litValue) {
        performLoadEdn2(memid, clock, sc, li)
      } else if (li.mod.litValue == StypeMod.FCN.litValue()) {
        performLoadFcn(memid, clock, sc, li)
      } else if (li.mod.litValue() == StypeMod.SEL.litValue()) {
        performLoadSel(memid, clock, sc, li)
      } else {
        throw new IllegalArgumentException(f"Unable to decode S-type load instruction. Mod was ${li.mod}")
      }
    }
  }

  /**
   * Wrapper function used to encapsulate all operations happening in the execute part of an instruction packet
   * @param idex Interface between ID and EX stages
   * @param wbid Interface between WB and ID stages
   * @param clock DUT Clock
   * @param sc The simulation container object
   */
  def handleExecuteInstruction(idex: IdExIO, wbid: WbIdIO, clock: Clock, sc: SimulationContainer): Unit = {
    var instrCnt = 0
    while(instrCnt < sc.iBuffer.exec.length) {
      handleMACSVandMACVV(idex, sc)

      //Using if(we) instead of while(!we) to ensure that we always clock once after observing a result
      if(wbid.we.peek.litToBoolean) {
        //MAC.VV, MAC.SV instructions only output on the final cycle of that packet.
        // Skip them while working towards the final output
        if(sc.iBuffer.exec(instrCnt).op.litValue == MAC.litValue
          && getResultRegisterType(sc.iBuffer.exec(instrCnt)).litValue == RegisterFileType.SREG.litValue
          && sc.progress != (sc.maxProgress - sc.progressIncr)) {
          instrCnt += 1
        }
        println(f"${sc.execThread} Executing instruction ${sc.iBuffer.exec(instrCnt)}")
        expectAndUpdate(wbid, idex, clock, sc, sc.iBuffer.exec(instrCnt))
        instrCnt += 1
      }
      clock.step()
    }
  }

  /**
   * Wrapper function used to encapsulate all operations happening in the memory store part of an instruction packet
   * @param idmem Interface between ID and MEM stages
   * @param clock DUT Clock
   * @param sc The simulation container object
   */
  def handleMemoryStoreOperations(idmem: IdMemIO, clock: Clock, sc: SimulationContainer): Unit = {
    //Wait until an operation is started on one of the three channels
    for(si <- sc.iBuffer.store) {
      while(!idmem.writeQueue.valid.peek.litToBoolean) {
        clock.step()
      }

      println(f"${sc.memThread} Storing instruction $si")
      if(si.mod.litValue == StypeMod.VEC.litValue) {
        performStoreVec(idmem, clock, sc, si)
      } else if (si.mod.litValue() == StypeMod.DOF.litValue) {
        performStoreDof(idmem, clock, sc, si)
      } else if (si.mod.litValue() == StypeMod.FDOF.litValue) {
        performStoreFdof(idmem, clock, sc, si)
      }else if (si.mod.litValue() == StypeMod.ELEM.litValue) {
        performStoreElem(idmem, clock, sc ,si)
      } else if (si.mod.litValue() == StypeMod.SEL.litValue()) {
        performStoreSel(idmem, clock, sc, si)
      } else {
        throw new IllegalArgumentException(s"Unable to decode S-type store instruction. Mod was ${si.mod}")
      }
    }
  }


  def performExecution(dut: TopLevel, sc: SimulationContainer): Unit = {
    sc.packetSetup()

    def perform(id: Int): Unit = {
      println(f"$id entering perform")
      while(!sc.isFinished) {
        println(f"$id starting load")
        handleMemoryLoadOperations(dut.io.memid, dut.clock, sc)
        println(f"$id load finished")
        sc.signalSectionFinished(id)
        while(!sc.awaitSwap(id)) {
          dut.clock.step()
        }
        println(f"$id starting exec")
        handleExecuteInstruction(dut.io.idex, dut.io.wbid, dut.clock, sc)
        sc.signalSectionFinished(id)
        println(f"$id exec finished")
        while(!sc.awaitSwap(id)) {
          dut.clock.step()
        }
        println(f"$id starting store")
        handleMemoryStoreOperations(dut.io.idmem, dut.clock, sc)
        sc.increaseMemoryBase()
        println(f"$id store finished")
      }
      println(f"$id exiting perform")
      sc.signalSectionFinished(id)
      sc.signalThreadFinished()
      while(!sc.allThreadsFinished) {
        dut.clock.step()
      }
      println("All threads finished")
    }

    fork {
      perform(0)
    } .fork {
      println(f"1 before perform")
      sc.signalSectionFinished(1) //This thread must wait until memory stage becomes available
      while(!sc.awaitSwap(1)) {
        dut.clock.step()
      }
      sc.increaseMemoryBase() //And must access elements further in the memory hierarchy
      perform(1)
    } .join()
    println("------------------------------------\n\n")
  }

  /**
   * Main function for testing the top-level component
   *
   * @param filename The program to execute
   * @param timeout The clock timeout value to be used
   * @param annos Annotations to pass to the tester
   * @param dumpMemory Whether to dump all memory contents to csv files after simulation is finished
   */
  def testFun(filename: String, timeout: Int = 500, annos: AnnotationSeq = Seq(), dumpMemory: Boolean = false): Unit = {
    val source = Source.fromFile(f"src/resources/programs/$filename.txt")
    Assembler.writeMemInitFile(f"src/resources/programs/$filename.hex.txt", Assembler.assemble(source))
    source.close()
    val wordsPerBank = SynthesisMemInit("src/resources/meminit")
    val sc = new SimulationContainer

    test(new TopLevel(IMsize = 128,
      IMinitFileLocation = s"src/resources/programs/$filename.hex.txt",
      wordsPerBank,
      memInitFileLocation = "src/resources/meminit")).withAnnotations(annos) {dut =>

      //Initial setup of SC and clock timeout
      sc.initialize(dut.decode, "src/resources/meminit")
      dut.clock.setTimeout(timeout)
      dut.clock.step() //1 cycle to get first instruction into decode stage
      assert(dut.io.idctrl.instr.peek.litValue != 0, "Peeked instruction with value 0, did not init memory correctly")

      while(dut.io.idctrl.instr.peek.litValue != 0) { //When instr==0, execution is finished
        //NOTE: Does not take into account branch instructions right now
        val instr = dut.io.idctrl.instr.peek()
        val fmt = InstructionFMT(instr(7,6).litValue.toInt)
        if(fmt.litValue == InstructionFMT.BTYPE.litValue()) {
          verifyBranchOutcome(dut.io.idctrl, dut.clock, BtypeInstruction(instr), sc.sReg)
        } else {
          fillInstructionBuffer(dut.io.idctrl, dut.clock, sc.iBuffer)
          performExecution(dut, sc)

          //Wait until all threads are idle
          while(dut.io.idctrl.stateUint.peek.litValue() != DecodeState.sIdle.litValue()) {
            dut.clock.step()
          }
          //Get console input, ask if we should dump contents or run the next N packets?
        }
      }
      print(f"\n\nSimulation finished")
      if(dumpMemory) dumpMemoryContents(filename, sc)

    }
  }

  it should "execute a simple program" in {
    testFun("simple", dumpMemory = true)
  }

  it should "perform applyStateOperator" in {
    testFun("applystateoperator", timeout=20000)
  }

  it should "perform applyDensityFilter" in {
    testFun("applydensityfilter", timeout=30000)
  }

  it should "perform getComplianceAndSensitivity" in {
    testFun("getComplianceAndSensitivity", 20000)
  }

  it should "perform generateMatrixDiagonal" in {
    testFun("generateMatrixDiagonal")
  }
}

package stages

import chisel3._
import chisel3.util.log2Ceil
import chiseltest._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import execution.RegisterFileType._
import execution.StypeBaseAddress._
import execution.StypeLoadStore._
import execution.StypeMod._
import execution._
import memory._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._

class DecodeMemorySpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode and memory stages"

  val wordsPerBank = 1024
  val memInitFileLocation = "src/resources/meminit"
  val mem: Array[Array[Long]] = Array.ofDim[Long](NUM_MEMORY_BANKS, wordsPerBank)

  /**
   * (Re)initializes the variable [[mem]] to a clean version that matches the memory contents at startup
   * when simulating.
   */
  def initMemory(): Unit = {
    for(bank <- 0 until NUM_MEMORY_BANKS) {
      for(word <- 0 until wordsPerBank) {
        mem(bank)(word) = double2fixed(word*NUM_MEMORY_BANKS + bank)
      }
    }
  }

  /**
   * Loads instructions into the DUT
   * @param in The input of the DUT, implementing [[IfIdIO]]
   * @param clock The clock of dut DUT
   * @param instrs The instructions to be loaded into the DUT
   */
  def loadInstructions(in: IfIdIO, clock: Clock, instrs: Array[Bundle with Instruction]): Unit = {
    for(instr <- instrs) {
      in.instr.poke(instr.toUInt())
      clock.step()
    }
    clock.step()
  }


  /**
   * Calculates the expected data for a given memory load operation
   * @param indices The memory indices that the load operation accessed / The IJK-values used for the load operation
   * @param baseAddr The S-type base address of the load operation
   * @return
   */
  def calculateExpectedData(indices: Seq[Int], baseAddr: StypeBaseAddress.Type): Seq[Long] = {
    //Decode base address to literal offset.
    val baseAddrDec = AddressDecode.mapping(baseAddr.litValue.toInt)
    val addresses = indices.map(_ + baseAddrDec)

    //Get from memory
    val rdData = Array.ofDim[Long](indices.length)
    for(i <- addresses.indices) {
      if(indices(i) < 0) {rdData(i) = 0} else {
        val bank = addresses(i) % NUM_MEMORY_BANKS
        val index = addresses(i) >> log2Ceil(NUM_MEMORY_BANKS)
        rdData(i) = mem(bank)(index)
      }
    }
    rdData
  }

  /**
   * Expects the output of a load instruction
   * @param expected A 2D container holding the expected data. expected(0) holds the data expected the first time we goes high, expected(1) the data expected the second time, etc.
   *                 expected(x)(0) should hold the data expected at wrData(0), expected(x)(1) the data at wrData(1), etc.
   * @param rd The base register where the load operation is supposed to happen. If expected.length > 0, it is assumed that a VEC or DOF load has been started, at which point the expected destination
   *           register will increment for each time we is enabled. If eg. rd=8 and VREG_SLOT_WIDTH=8, the expected registers will be 8,9,...,15.
   * @param wb The writeback port of the DUT
   * @param clock The clock of the DUT
   * @note If expected(x).length > XREG_DEPTH, it is assumed that the destination register is the vector register file. If expected(x).length <= XREG_DEPTH, it is assumed that the destination register if the x register file.
   */
  def expectLoad(expected: Seq[Seq[Long]], rd: Int, wb: WbIdIO, clock: Clock): Unit = {
    for(i <- expected.indices) {
      while(!wb.we.peek.litToBoolean) { //Step up to next result
        clock.step()
      }
      val exp = expected(i)
      val reg = rd+i
      val rf = if(exp.length > XREG_DEPTH) VREG else XREG
      for(j <- exp.indices) {
        wb.wrData(j).expect(exp(j).S)
      }
      wb.rd.reg.expect(reg.U)
      wb.rd.rf.expect(rf)
      clock.step() //And step past it
    }
  }

  private val annos = Seq(WriteVcdAnnotation)
  it should "perform a ld.sel" in {
    /*
    pstart
    ld.sel x0, X
    estart
    eend
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, ld.sel")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) { dut =>
      dut.clock.setTimeout(30)
      val instr = StypeInstruction(rsrd=0, mod=SEL, baseAddr=X, ls=LOAD)
      val ijk = Array(0,0,0)
      val expData = calculateExpectedData(Seq(elementIndex(ijk)), baseAddr=X)
      val instrs = wrapLoadStoreInstructions(Array(instr))

      loadInstructions(dut.io.in, dut.clock, instrs)
      var x = 0
      while(!dut.io.memWb.we.peek.litToBoolean) {
        dut.clock.step()
        x += 1
      }
      dut.io.memWb.wrData(0).expect(expData(0).S)
      dut.io.memWb.rd.reg.expect(0.U)
      dut.io.memWb.rd.rf.expect(XREG)
    }
  }


  it should "perform st.sel" in {
    /*
    pstart single
    ld.sel x0, X
    ld.sel x1, XPHYS
    estart
    eend
    st.sel x0, XPHYS
    st.sel x1, X
    pend
    pstart single
    ld.sel x0, X
    ld.sel x1, XPHYS
    estart
    eend
    pend
     */
    //Load the values from X, XPHYS and store them back into their respective memories.
    //On the next instruction, read the newly stored values
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, st.sel")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      dut.clock.setTimeout(25)
      val ijk = Array(0,0,0,0)
      val ldInstr = Array(StypeInstruction(rsrd=0, mod=SEL, baseAddr=X, ls=LOAD), StypeInstruction(rsrd=1, mod=SEL, baseAddr=XPHYS, ls=LOAD))
      val stInstr = Array(StypeInstruction(rsrd=0, mod=SEL, baseAddr=XPHYS, ls=STORE), StypeInstruction(rsrd=1, mod=SEL, baseAddr = X, ls=STORE))
      val expX = calculateExpectedData(Seq(elementIndex(ijk)), baseAddr=X)
      val expXphys = calculateExpectedData(Seq(elementIndex(ijk)), baseAddr=XPHYS)
      val p1 = wrapLoadStoreInstructions(ldInstr, Some(stInstr))
      val p2 = wrapLoadStoreInstructions(ldInstr)

      //Perform load/store handshake
      //Simply step through this instruction, we won't bother observing the transaction
      loadInstructions(dut.io.in, dut.clock, p1)
      dut.clock.step()
      while(dut.io.idStateUint.peek.litValue != DecodeState.sIdle.litValue()) {
        dut.clock.step()
      }

      //Perform load instruction
      loadInstructions(dut.io.in, dut.clock, p2)
      //Observe value in X that came from XPHYS
      while(!dut.io.memWb.we.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.io.memWb.wrData(0).expect(expXphys(0).S)
      dut.io.memWb.rd.reg.expect(0.U)
      //Observe value in XPHYS that came from X
      dut.clock.step()
      while(!dut.io.memWb.we.peek().litToBoolean) {
        dut.clock.step()
      }
      dut.io.memWb.wrData(0).expect(expX(0).S)
      dut.io.memWb.rd.reg.expect(1.U)
    }
  }

  it should "perform at st.elem" in {
    /*
   pstart single
   ld.elem x0, X
   ld.elem x1, XPHYS
   estart
   eend
   st.elem x0, XPHYS
   st.elem x1, X
   pend
   pstart single
   ld.elem x0, X
   ld.elem x1, XPHYS
   estart
   eend
   pend
    */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, st.elem")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      dut.clock.setTimeout(100)
      val ijk = genIJKmultiple(start=Some(Array(0,0,0,0)))
      val ldInstr = Array(StypeInstruction(rsrd=0, mod=ELEM, baseAddr=X, ls=LOAD), StypeInstruction(rsrd=1, mod=ELEM, baseAddr=XPHYS, ls=LOAD))
      val stInstr = Array(StypeInstruction(rsrd=0, mod=ELEM, baseAddr=XPHYS, ls=STORE), StypeInstruction(rsrd=1, mod=ELEM, baseAddr = X, ls=STORE))
      val expX = calculateExpectedData(ijk.map(elementIndex), X)
      val expXphys = calculateExpectedData(ijk.map(elementIndex), XPHYS)
      val p1 = wrapLoadStoreInstructions(ldInstr, Some(stInstr))
      val p2 = wrapLoadStoreInstructions(ldInstr)

      //Perform load/store handshake
      //Simply step through this instruction, we won't bother observing the transaction
      loadInstructions(dut.io.in, dut.clock, p1)
      dut.clock.step()
      while(dut.io.idStateUint.peek.litValue != DecodeState.sIdle.litValue()) {
        dut.clock.step()
      }

      //Perform load instruction
      loadInstructions(dut.io.in, dut.clock, p2)
      //Observe value in X that came from XPHYS
      while(!dut.io.memWb.we.peek().litToBoolean) {
        dut.clock.step()
      }
      for(i <- expXphys.indices) {
        dut.io.memWb.wrData(i).expect(expXphys(i).S)
      }
      dut.io.memWb.rd.reg.expect(0.U)
      //Observe value in XPHYS that came from X
      dut.clock.step()
      while(!dut.io.memWb.we.peek().litToBoolean) {
        dut.clock.step()
      }
      for(i <- expX.indices) {
        dut.io.memWb.wrData(i).expect(expX(i).S)
      }
      dut.io.memWb.rd.reg.expect(1.U)
    }
  }

  it should "perform a ld.elem" in {
    /*
    pstart
    ld.elem x1, Q
    estart
    eend
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, ld.elem")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      dut.clock.setTimeout(30)
      val rd = scala.util.Random.nextInt(NUM_XREG)
      val ijk = genIJKmultiple(start=Some(Array(0,0,0,0)))
      val expData = calculateExpectedData(ijk.map(elementIndex), baseAddr=Q)

      val instr = StypeInstruction(rd, mod=ELEM, baseAddr=Q, ls=LOAD)
      val instrs = wrapLoadStoreInstructions(Array(instr))

      loadInstructions(dut.io.in, dut.clock, instrs)
      while(!dut.io.memWb.we.peek.litToBoolean) {
        dut.clock.step()
      }
      for(i <- expData.indices) {
        dut.io.memWb.wrData(i).expect(expData(i).S)
      }
      dut.io.memWb.rd.reg.expect(rd.U)
      dut.io.memWb.rd.rf.expect(XREG)
    }
  }

  it should "perform a ld.dof followed by ld.elem" in {
    /*
    pstart
    ld.dof v0, X
    ld.elem x0, X
    estart
    eend
    peend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, ld.dof + ld.elem")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      dut.clock.setTimeout(50)
      val vrd = 0
      val xrd = 0
      val baseAddr = X
      val dof = StypeInstruction(vrd, mod=DOF, baseAddr, ls=LOAD)
      val elem = StypeInstruction(xrd, mod=ELEM, baseAddr, ls=LOAD)
      val instrs = Array(dof, elem)
      val packet = wrapLoadStoreInstructions(Array(dof, elem))

      val ijk = genIJKmultiple(start=Some(Array(0,0,0,0)))
      val edof = ijk.map(e => getEdof(e(0), e(1), e(2)))
      val expDof = edof.map(e => calculateExpectedData(e, baseAddr))
      val expElem = calculateExpectedData(ijk.map(elementIndex), baseAddr)

      loadInstructions(dut.io.in, dut.clock, packet)
      expectLoad(expDof, vrd, wb=dut.io.memWb, clock=dut.clock)
      expectLoad(Seq(expElem), xrd, dut.io.memWb, dut.clock)
    }
  }

  it should "perform a st.dof" in {
    /*
    pstart single
    ld.dof v0, F
    ld.dof v1, U
    estart
    eend
    st.dof v0, U
    st.dof v1, F
    pend
    pstart single
    ld.dof v0, F
    ld.dof v1, U
    estart
    end
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, st.dof")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) { dut =>
      dut.clock.setTimeout(150)
      val ijk = genIJKmultiple(start = Some(Array(0, 0, 0, 0)))
      val edof = ijk.map(i => getEdof(i(0), i(1), i(2)))
      val ldInstr = Array(StypeInstruction(rsrd = 0, mod = DOF, baseAddr = F, ls = LOAD), StypeInstruction(rsrd = 1, mod = DOF, baseAddr = U, ls = LOAD))
      val stInstr = Array(StypeInstruction(rsrd = 0, mod = DOF, baseAddr = U, ls = STORE), StypeInstruction(rsrd = 1, mod = DOF, baseAddr = F, ls = STORE))

      val expF = edof.map(i => calculateExpectedData(i, F))
      val expU = edof.map(i => calculateExpectedData(i, U))

      val p1 = wrapLoadStoreInstructions(ldInstr, Some(stInstr), len = OtypeLen.SINGLE)
      val p2 = wrapLoadStoreInstructions(ldInstr, len = OtypeLen.SINGLE)

      //Perform load/store handshake
      //Simply step through this instruction, we won't bother observing the transaction
      loadInstructions(dut.io.in, dut.clock, p1)
      dut.clock.step()
      while (dut.io.idStateUint.peek.litValue != DecodeState.sIdle.litValue()) {
        dut.clock.step()
      }

      //Perform load instruction
      loadInstructions(dut.io.in, dut.clock, p2)

      //Observe value in F that came from U
      for (i <- expU.indices) {
        while (!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expU(i)
        for (j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S)
        }
        dut.clock.step()
      }
      for (i <- expF.indices) {
        while (!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expF(i)
        for (j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S)
        }
        dut.clock.step()
      }
    }
  }

  it should "perform a st.fdof" in {
    /*
   pstart single
   ld.dof v0, F
   ld.dof v1, U
   estart
   eend
   st.fdof v0, U
   st.fdof v1, F
   pend
   pstart single
   ld.dof v0, F
   ld.dof v1, U
   estart
   end
   pend
  */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, st.fdof")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) { dut =>
      dut.clock.setTimeout(250)
      val ijk = genIJKmultiple(start = Some(Array(0, 0, 0, 0)))
      val edof = ijk.map(i => getEdof(i(0), i(1), i(2)))
      val ldInstr = Array(StypeInstruction(rsrd = 0, mod = DOF, baseAddr = F, ls = LOAD), StypeInstruction(rsrd = 1, mod = DOF, baseAddr = U, ls = LOAD))
      val stInstr = Array(StypeInstruction(rsrd = 0, mod = FDOF, baseAddr = U, ls = STORE), StypeInstruction(rsrd = 1, mod = FDOF, baseAddr = F, ls = STORE))

      val readF = edof.map(i => calculateExpectedData(i, F))
      val readU = edof.map(i => calculateExpectedData(i, U))
      val expF = edof.map(i => calculateExpectedData(i, F).toArray)
      val expU = edof.map(i => calculateExpectedData(i, U).toArray)

      //Expected data is not a copy of read data since we're performing fixed dof store
      //For each ijk pair, if i==0 (element is in bottom layer), the store operation will happen at indices 0-3, 8-11 and 16-19
      //The below code swaps the data between the two expected data arrays
      for(j <- expF.indices) {
        val rF = readF(j)
        val eF = expF(j)
        val rU = readU(j)
        val eU = expU(j)
        val i = ijk(j)
        if(i(0) == 0) {
          for(k <- 0 until 3) {
            for(l <- 0 until 4) {
              eF(k*NUM_MEMORY_BANKS+l) = rU(k*NUM_MEMORY_BANKS+l)
              eU(k*NUM_MEMORY_BANKS+l) = rF(k*NUM_MEMORY_BANKS+l)
            }
          }
        }
      }

      val p1 = wrapLoadStoreInstructions(ldInstr, Some(stInstr), len = OtypeLen.SINGLE)
      val p2 = wrapLoadStoreInstructions(ldInstr, len = OtypeLen.SINGLE)

      //Perform load/store handshake
      //Simply step through this instruction, we won't bother observing the transaction
      loadInstructions(dut.io.in, dut.clock, p1)
      dut.clock.step()
      while (dut.io.idStateUint.peek.litValue != DecodeState.sIdle.litValue()) {
        dut.clock.step()
      }

      //Perform load instruction
      loadInstructions(dut.io.in, dut.clock, p2)

      //Observe value in F that came from U
      for (i <- expF.indices) {
        while (!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expF(i)
        for (j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S) //io_memWb_wrData_0=73924608 (0x4680000) did not equal expected=40108032 (0x2640000) (lines in DecodeMemorySpec.scala: 455, 450, 404)
        }
        dut.clock.step()
      }
      for (i <- expU.indices) {
        while (!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expU(i)
        for (j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S)
        }
        dut.clock.step()
      }
    }
  }

  it should "perform loads around a central element" in {
    /*
    pstart
    ld.fcn x0, X
    ld.edn1 x1, X
    ld.edn2 x2, X
    ld.sel x3, X
    estart
    eend
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, central element loads")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      //Define instructions and calculate expected outputs
      val baseAddr = X
      val fcn = StypeInstruction(rsrd=0, mod=FCN, baseAddr)
      val edn1 = StypeInstruction(rsrd=1, mod=EDN1, baseAddr)
      val edn2 = StypeInstruction(rsrd=2, mod=EDN2, baseAddr)
      val sel = StypeInstruction(rsrd=3, mod=SEL, baseAddr)
      val instrs = Array(fcn, edn1, edn2, sel)
      val packet = wrapLoadStoreInstructions(instrs)

      val ijk = Array(0,0,0,0)
      val expFcn = calculateExpectedData(getFcnIndices(ijk), baseAddr)
      val edn1Fcn = calculateExpectedData(getEdn1Indices(ijk), baseAddr)
      val expEdn2 = calculateExpectedData(getEdn2Indices(ijk), baseAddr)
      val expSel = Seq(calculateExpectedData(Seq(elementIndex(ijk)), baseAddr), Seq.fill(XREG_DEPTH-1)(0L)).flatten //Must also receive 0's on the remaining positions

      //Execution
      loadInstructions(dut.io.in, dut.clock, packet)
      expectLoad(expected=Seq(expFcn), rd=0, wb=dut.io.memWb, clock=dut.clock)
      expectLoad(Seq(edn1Fcn), 1, dut.io.memWb, dut.clock)
      expectLoad(Seq(expEdn2), 2, dut.io.memWb, dut.clock)
      expectLoad(Seq(expSel), 3, dut.io.memWb, dut.clock)
    }
  }

  it should "perform ld.vec" in {
    /*
    pstart
    ld.vec v0, P
    ld.vec v2, Q
    estart
    eend
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, ld.vec")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      //Setup, define instructions and expected data
      val baseAddr = randomElement(baseAddresses)
      val rd1 = 0
      val rd2 = 2
      val vec1 = StypeInstruction(rsrd=rd1, mod=VEC, P)
      val vec2 = StypeInstruction(rsrd=rd2, mod=VEC, Q)
      val instrs = Array(vec1, vec2)
      val packet = wrapLoadStoreInstructions(instrs)

      val indices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(n*VREG_DEPTH, (n+1)*VREG_DEPTH))
      val expVec1 = indices.map(i => calculateExpectedData(i, vec1.baseAddr))
      val expVec2 = indices.map(i => calculateExpectedData(i, vec2.baseAddr))

      //Execution
      loadInstructions(dut.io.in, dut.clock, packet)
      expectLoad(expected=expVec1, rd=rd1, wb=dut.io.memWb, clock=dut.clock)
      expectLoad(expVec2, rd2*VREG_SLOT_WIDTH, dut.io.memWb, dut.clock)
    }
  }

  it should "perform a st.vec" in {
    /*
    pstart single
    ld.vec v0, P
    ld.vec v1, Q
    estart
    eend
    st.vec v0, Q
    st.vec v1, P
    pend
    pstart single
    ld.vec v0, P
    ld.vec v1, Q
    estart
    eend
    pend
     */
    simulationConfig(true)
    initMemory()
    seed("Decode and memory, st.vec")
    test(new DecodeMemory(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
//      dut.clock.setTimeout(200)
      val indices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(n*VREG_DEPTH, (n+1)*VREG_DEPTH))
      val ldInstr = Array(StypeInstruction(rsrd=0, mod=VEC, baseAddr=P, ls=LOAD), StypeInstruction(rsrd=1, mod=VEC, baseAddr=Q, ls=LOAD))
      val stInstr = Array(StypeInstruction(rsrd=0, mod=VEC, baseAddr=Q, ls=STORE), StypeInstruction(rsrd=1, mod=VEC, baseAddr = P, ls=STORE))

      val expP = indices.map(i => calculateExpectedData(i, P))
      val expQ = indices.map(i => calculateExpectedData(i, Q))

      val p1 = wrapLoadStoreInstructions(ldInstr, Some(stInstr), len=OtypeLen.SINGLE)
      val p2 = wrapLoadStoreInstructions(ldInstr, len=OtypeLen.SINGLE)

      //Perform load/store handshake
      //Simply step through this instruction, we won't bother observing the transaction
      loadInstructions(dut.io.in, dut.clock, p1)
      dut.clock.step()
      while(dut.io.idStateUint.peek.litValue != DecodeState.sIdle.litValue()) {
        dut.clock.step()
      }

      //Perform load instruction
      loadInstructions(dut.io.in, dut.clock, p2)

      //Observe value in P that came from Q

      for(i <- expQ.indices) {
        while(!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expQ(i)
        for(j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S)
        }
        dut.clock.step()
      }
      for(i <- expP.indices) {
        while(!dut.io.memWb.we.peek().litToBoolean) {
          dut.clock.step()
        }
        val e = expP(i)
        for(j <- e.indices) {
          dut.io.memWb.wrData(j).expect(e(j).S)
        }
        dut.clock.step()
      }
    }
  }

  it should "disallow ld.fdof, st.edn1, st.edn2 and st.fcn instructions" in {
    try {
      StypeInstruction(rsrd=0, mod=FDOF, baseAddr=X, ls=LOAD)
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.toLowerCase.contains("cannot")) assert(true) else assert(false)
    }

    try {
      StypeInstruction(rsrd=0, mod=EDN1, baseAddr=X, ls=STORE)
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.toLowerCase.contains("cannot")) assert(true) else assert(false)
    }

    try {
      StypeInstruction(rsrd=0, mod=EDN2, baseAddr=X, ls=STORE)
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.toLowerCase.contains("cannot")) assert(true) else assert(false)
    }

    try {
      StypeInstruction(rsrd=0, mod=FCN, baseAddr=X, ls=STORE)
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.toLowerCase.contains("cannot")) assert(true) else assert(false)
    }
  }



}

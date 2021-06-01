package pipeline

import chisel3._
import chisel3.util.log2Ceil
import org.scalatest.{FlatSpec, Matchers}
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.experimental.TestOptionBuilder._
import memory.{AddressDecode, baseAddresses, elementIndex, genIJK, genIJKmultiple, getEdn1Indices, getEdn2Indices, getEdof, getFcnIndices, randomElement}
import pipeline.StypeMod._
import pipeline.StypeBaseAddress._
import pipeline.StypeLoadStore._
import utils.Config._
import utils.Fixed._
import pipeline.RegisterFileType._

class DecodeMemorySpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Decode and memory stages"

  val wordsPerBank = 1024
  val memInitFileLocation = "src/test/scala/memory/membankinit"
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
      val instrs = wrapLoadInstructions(Array(instr))

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
      val instrs = wrapLoadInstructions(Array(instr))

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
      dut.clock.setTimeout(30)
      val vrd = 0
      val xrd = 0
      val baseAddr = X
      val dof = StypeInstruction(vrd, mod=DOF, baseAddr, ls=LOAD)
      val elem = StypeInstruction(xrd, mod=ELEM, baseAddr, ls=LOAD)
      val instrs = Array(dof, elem)
      val packet = wrapLoadInstructions(Array(dof, elem))

      val ijk = genIJKmultiple(start=Some(Array(0,0,0,0)))
      val edof = ijk.map(e => getEdof(e(0), e(1), e(2)))
      val expDof = edof.map(e => calculateExpectedData(e, baseAddr))
      val expElem = calculateExpectedData(ijk.map(elementIndex), baseAddr)

      loadInstructions(dut.io.in, dut.clock, packet)
      expectLoad(expDof, vrd, wb=dut.io.memWb, clock=dut.clock)
      expectLoad(Seq(expElem), xrd, dut.io.memWb, dut.clock)
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
      val packet = wrapLoadInstructions(instrs)

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
      val packet = wrapLoadInstructions(instrs)

      val indices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(n*VREG_DEPTH, (n+1)*VREG_DEPTH))
      val expVec1 = indices.map(i => calculateExpectedData(i, vec1.baseAddr))
      val expVec2 = indices.map(i => calculateExpectedData(i, vec2.baseAddr))

      //Execution
      loadInstructions(dut.io.in, dut.clock, packet)
      expectLoad(expected=expVec1, rd=rd1, wb=dut.io.memWb, clock=dut.clock)
      expectLoad(expVec2, rd2*VREG_SLOT_WIDTH, dut.io.memWb, dut.clock)
    }
  }


}

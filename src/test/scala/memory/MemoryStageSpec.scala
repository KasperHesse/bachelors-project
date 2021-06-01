package memory

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import pipeline.RegisterFileType.SREG
import pipeline.{RegisterBundle, RegisterFileType, StypeBaseAddress, StypeMod, WbIdIO, seed}
import pipeline.StypeMod._
import pipeline.StypeBaseAddress._
import pipeline.RegisterFileType._

class MemoryStageSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Memory stage"

  val wordsPerBank = 1300
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

  def pokeReadQueue(dut: MemoryStage, rd: RegisterBundle, iter: Int, mod: StypeMod.Type): Unit = {
    dut.io.id.readQueue.bits.rd.poke(rd)
    dut.io.id.readQueue.bits.iter.poke(iter.U)
    dut.io.id.readQueue.bits.mod.poke(mod)
    dut.io.id.readQueue.valid.poke(true.B)
  }

  /**
   * Pokes the VEC inputs of the memory stage.
   * @param dut The DUT
   * @param indices The indices to poke. If indices.length < NUM_MEMORY_BANKS, the remaining indices are set false
   * @param baseAddr The base address to load from.
   */
  def pokeVec(dut: MemoryStage, indices: Seq[Int], baseAddr: StypeBaseAddress.Type = StypeBaseAddress.KE): Unit = {
    for(i <- indices.indices) {
      dut.io.id.vec.bits.indices(i).poke(indices(i).U)
      dut.io.id.vec.bits.validIndices(i).poke(true.B)
    }
    for(i <- indices.length until NUM_MEMORY_BANKS) {
      dut.io.id.vec.bits.indices(i).poke(0.U)
      dut.io.id.vec.bits.validIndices(i).poke(false.B)
    }
    dut.io.id.vec.bits.baseAddr.poke(baseAddr)
    dut.io.id.vec.valid.poke(true.B)
  }

  /**
   * Pokes an IJK-value onto the DUT.
   * @param dut The DUT
   * @param ijk An array holding at (0) the i-value, at (1) the j-value and at (2) the k-value.
   * @param baseAddr The S-type base address of the load being performed
   * @param mod The S-type modifier of the instruction being processed. Defaults to DOF.
   * @param pad Whether this ijk-bundle should be seen as padding or interpreted literally. Defaults to false (not padding)
   */
  def pokeIJK(dut: MemoryStage, ijk: Array[Int], baseAddr: StypeBaseAddress.Type, mod: StypeMod.Type = DOF, pad: Boolean = false): Unit = {
    dut.io.id.neighbour.bits.ijk.poke((new IJKBundle).Lit(_.i -> ijk(0).U, _.j -> ijk(1).U, _.k -> ijk(2).U))
    dut.io.id.neighbour.bits.baseAddr.poke(baseAddr)
    dut.io.id.neighbour.bits.pad.poke(pad.B)
    dut.io.id.neighbour.bits.mod.poke(mod)
    dut.io.id.neighbour.valid.poke(true.B)
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
   * Expects output data from the memory stage. Expects data(0) to be presented at wrData(0), data(1) at wrData(1) etc.
   * If data.length < wrData.length, the remaining indices will not be checked.
   * If rd is not explicitly set, it will not be checked.
   * Finishes by checking if we is asserted
   * @param dut The DUT
   * @param expData The data to be expected on the output
   * @param rd The destination register to be expected. Default value is null. Will not be checked if null.
   */
  def expectData(dut: MemoryStage, expData: Seq[Long], rd: RegisterBundle = null): Unit = {
    //Data re-ordering will sometimes mess us up. Instead, let's try something slightly different
    for(i <- expData.indices) {
      dut.io.wb.wrData(i).expect(expData(i).S)
    }
    if(rd != null) {
      dut.io.wb.rd.expect(rd)
    }
    dut.io.wb.we.expect(true.B)
  }

  /**
   * Performs the logic necessary for a single ld.vec operation
   * @param dut The DUT
   */
  def performLdVec(dut: MemoryStage): Unit = {
    val startIndex = scala.util.Random.nextInt((NDOF - VREG_DEPTH) / NUM_MEMORY_BANKS) * NUM_MEMORY_BANKS
    val indices = Seq.range(startIndex, startIndex + VREG_DEPTH)
    val rd = (new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> RegisterFileType.VREG, _.rfUint -> 0.U, _.subvec -> 0.U)
    val baseAddr = randomElement(baseAddresses)
    val expData = calculateExpectedData(indices, baseAddr)

    var pokeCnt = 0
    val pokeMax = VREG_DEPTH / NUM_MEMORY_BANKS
    var readCnt = 0
    val readMax = 1
    var iter = 0
    val iterMax = 10

    while (readCnt < readMax  && iter < iterMax) {
      pokeVec(dut, indices.slice(pokeCnt * NUM_MEMORY_BANKS, (pokeCnt + 1) * NUM_MEMORY_BANKS), baseAddr)
      pokeReadQueue(dut, rd, 0, VEC)
      if (dut.io.id.vec.ready.peek.litToBoolean && pokeCnt < pokeMax) {
        pokeCnt += 1
      } else {
        dut.io.id.readQueue.valid.poke(false.B)
        dut.io.id.vec.valid.poke(false.B)
      }
      dut.clock.step()
      if(dut.io.wb.we.peek.litToBoolean) {
        expectData(dut, expData, rd)
        readCnt += 1
      }
      iter += 1
    }
    assert(readCnt == readMax)
  }

  def performStVec(dut: MemoryStage): Unit = {

  }

  /**
   * Performs the logic necessary for a ld.dof operation
   * @param dut The DUT
   * @param pad Whether to assert the 'pad' flag or not
   */
  def performLdDof(dut: MemoryStage, pad: Boolean): Unit = {
    val ijk = genIJK()
    val edof = getEdof(ijk(0), ijk(1), ijk(2))
    val rdReg = scala.util.Random.nextInt(NUM_VREG_SLOTS)
    val rd = (new RegisterBundle).Lit(_.reg -> rdReg.U, _.rf -> RegisterFileType.VREG, _.rfUint -> 0.U, _.subvec -> 0.U)
    val baseAddr = randomElement(baseAddresses)
    def expData = if(pad) Seq.fill(24)(0L) else calculateExpectedData(edof, baseAddr)

    var pokeCnt = 0
    var readCnt = 0
    while(readCnt < 1) {
      if (pokeCnt == 0) pokeIJK(dut, ijk, baseAddr, pad=pad) else dut.io.id.neighbour.valid.poke(false.B)
      pokeReadQueue(dut, rd, ijk(3), DOF)
      if ((dut.io.id.neighbour.ready.peek.litToBoolean || pokeCnt > 0) && pokeCnt < 3) { //As soon as pokeCnt > 0, we have latched in ijk-values. Keep incrementing to poke readQueue thrice
        pokeCnt += 1
      } else {
        dut.io.id.readQueue.valid.poke(false.B)
      }
      dut.clock.step()
      if (dut.io.wb.we.peek.litToBoolean) {
        expectData(dut, expData, rd)
        readCnt += 1
      }
    }
  }

  /**
   * Performs some generic setup on the ready/valid interfaces of the memory stage, to make later processing easier
   * @param dut
   */
  def setupClock(dut: MemoryStage): Unit = {
    dut.io.id.vec.initSource().setSourceClock(dut.clock)
    dut.io.id.readQueue.initSource().setSourceClock(dut.clock)
    dut.io.id.neighbour.initSource().setSourceClock(dut.clock)
    dut.io.id.edof.initSource().setSourceClock(dut.clock)
  }

  /**
   * Generates a read queue bundle for poking onto the DUT
   * @param reg The destination register index of the instruction
   * @param rf The destination register file of the instruction
   * @param iter The ijk-iteration value, if relevant
   * @param mod The S-type modifier of the instruction
   * @return A ReadQueueBundle holding all of the above information
   */
  def genReadQueueBundle(reg: Int, rf: RegisterFileType.Type, iter: Int, mod: StypeMod.Type): ReadQueueBundle = {
    val rd = (new RegisterBundle).Lit(_.reg -> reg.U, _.rf -> rf, _.rfUint -> rf.litValue.toInt.U, _.subvec -> 0.U)
    (new ReadQueueBundle).Lit(_.rd -> rd, _.iter -> iter.U, _.mod -> mod)
  }

  /**
   * Generates an IJK input bundle for poking onto the DUT
   * @param IJK An Option holding the IJK coordinates of the instruction. If None is given, generates a random ijk-coordinate pair. Defaults to None
   * @param baseAddress An Option holding the base address of the instruction. If None is given, selects a random base address. Defaults to None
   * @param pad Padding flag
   * @param mod S-type modifier for the instruction
   * @return An IJKGeneratorConsumerIO Bundle, ready to poke onto the IJK input port of the memory stage
   */
  def genIJKinput(IJK: Option[Array[Int]] = None, baseAddress: Option[StypeBaseAddress.Type] = None, pad: Boolean, mod: StypeMod.Type): IJKgeneratorConsumerIO = {
    val ijk = IJK match {
      case Some(x) => x
      case None => genIJK()
    }
    val baseAddr = baseAddress match {
      case Some(x) => x
      case None => randomElement(baseAddresses)
    }
    val ijkBundle = (new IJKBundle).Lit(_.i -> ijk(0).U, _.j -> ijk(1).U, _.k -> ijk(2).U)
    (new IJKgeneratorConsumerIO).Lit(_.baseAddr -> baseAddr, _.ijk -> ijkBundle, _.pad -> pad.B, _.mod -> mod)
  }

//  /**
//   * Generates a number of VEC input bundles for poking onto the DUT. Given 24 indices into a vector, this method
//   * returns a sequence of 3 bundles that can be used to execute that load operation.
//   * @param indices The 24 indices to load data from
//   * @param baseAddress The encoded base address from which data should be loaded
//   * @return A Seq containing 3 bundles that represent the desired loads
//   */
  //This won't work until Chisel 3.5 when vec literals are introduced
//  def genVecInput(indices: Seq[Int], baseAddress: Option[StypeBaseAddress.Type]): Seq[AddressGenProducerIO] = {
//    val baseAddr = baseAddress match {
//      case Some(x) => x
//      case None => randomElement(baseAddresses)
//    }
//    val seq = Seq.fill(3)((new AddressGenProducerIO).Lit(_.baseAddr -> baseAddr))
//    for(i <- 0 until 2) {
//      for(j <- 0 until 8) {
//        val ind = indices.slice(i*8, (i+1)*8)
//        seq(i).indices
//        seq(i).indices(j) = ind(j).U
//        seq(i).validIndices(j) = true.B
//      }
//    }
//  }

  /**
   * Forks a new thread which will poll for we to be asserted, and then expects the given data on the output
   * @param dut The DUT
   * @param expData The output data to be expected
   * @param rd The destination register to be expected
   */
  def waitAndExpect(dut: MemoryStage, expData: Seq[Long], rd: RegisterBundle): Unit = {
    fork {
      while(!dut.io.wb.we.peek.litToBoolean) {
        dut.clock.step()
      }
      expectData(dut, expData, rd)
    }.joinAndStep(dut.clock)
  }

  /** Creates a new sequence by taking every N'th element of an existing sequence.
   * From stackoverflow/25227475 */
  def takeNth[T](s: Seq[T], n: Int): Seq[T] = {
    s.zipWithIndex.collect {case (e,i) if ((i+1)%n) == 0 => e}
  }

  val annos = Seq.empty[firrtl.annotations.Annotation]
//  val annos = Seq(WriteVcdAnnotation)

  it should "perform a ld.vec" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.vec")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)
      performLdVec(dut)

//      val indices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(VREG_SLOT_WIDTH*n,VREG_SLOT_WIDTH*(n+1)))
//      val baseAddress = randomElement(baseAddresses)
//      val vec = indices.map(a => genVecInput)
    }
  }

  it should "perform at st.vec" in {

  }


  it should "zero out remaining bits in ld.vec" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.vec with final zeros")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      dut.clock.setTimeout(20)
      val startIndex = scala.util.Random.nextInt((NDOF-VREG_DEPTH)/NUM_MEMORY_BANKS)*NUM_MEMORY_BANKS
      val indices = Seq.range(startIndex, startIndex+VREG_DEPTH-4) //Final 4 values should be zeros
      val rd = (new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> RegisterFileType.VREG, _.rfUint -> 0.U, _.subvec -> 0.U)
      val baseAddr = randomElement(baseAddresses)

      var pokeCnt = 0
      while(pokeCnt < VREG_DEPTH/NUM_MEMORY_BANKS) {
        pokeVec(dut, indices.slice(pokeCnt * NUM_MEMORY_BANKS, (pokeCnt + 1) * NUM_MEMORY_BANKS), baseAddr)
        pokeReadQueue(dut, rd, 0, VEC)
        if(dut.io.id.vec.ready.peek.litToBoolean) {
          pokeCnt += 1
        } else {
          print(s"vec was not ready ...\n")
          dut.io.id.readQueue.valid.poke(false.B)
        }
        dut.clock.step()
      }
      dut.io.id.vec.valid.poke(false.B)
      while(!dut.io.wb.we.peek.litToBoolean) {
        dut.clock.step()
      }
      val expData = calculateExpectedData(indices, baseAddr)
      expectData(dut, expData, rd)
      for(i <- VREG_DEPTH-4 until VREG_DEPTH) {
        dut.io.wb.wrData(i).expect(0.S)
      }
    }
  }

  it should "perform a ld.dof" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.dof")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(30)

      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)))
      val baseAddress = randomElement(baseAddresses)
      val dof = ijk.map(a => genIJKinput(IJK = Some(a),  Some(baseAddress), pad=false, mod=DOF))
      val rdDof = dof.zipWithIndex.flatMap(d => Seq.fill(3)(genReadQueueBundle(d._2, rf = VREG, iter = ijk(d._2)(3), mod = DOF)))
      val expDof = getEdof(dof).map(e => calculateExpectedData(e, baseAddress))

      //Poke ijk data and readqueue data at the same time
      fork {
        dut.io.id.edof.enqueueSeq(dof)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(rdDof)
      }
      (expDof, takeNth(rdDof, 3)).zipped.foreach((e, r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "perform a ld.elem" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.elem")
    //Try generating multiple IJK values
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)
      //Generate ijk-values and their corresponding iteration numbers. Poke this onto the DUT in a linear fashion
      val ijkVals = genIJKmultiple()
      val ijk = ijkVals.map(a => genIJKinput(Some(a), Some(StypeBaseAddress.KE), pad=false, mod=ELEM))
      //Indices accessed are based on iteration values
      val rdq = ijkVals.map(a => genReadQueueBundle(reg=0, rf=XREG, iter=a(3), mod=ELEM))
      val expData = calculateExpectedData(ijkVals.map(elementIndex), baseAddr = KE)

      //Poke input data and read queue
      fork {
        dut.io.id.neighbour.enqueueSeq(ijk)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      waitAndExpect(dut, expData, rdq(0).rd)
    }
  }

  it should "perform a ld.sel" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.sel")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)
      //Generate an ijk val
      val ijkVal = genIJK()
      //Generate the instruction packet
      val ijk = genIJKinput(IJK = Some(ijkVal), pad=false, mod=SEL)
      //Read queue entry
      val rdq = genReadQueueBundle(reg=1, rf=XREG, iter=ijkVal(3), mod=SEL)
      //Expected data
      val expData = calculateExpectedData(Seq(elementIndex(ijkVal)), ijk.baseAddr)

      fork {
        dut.io.id.neighbour.enqueue(ijk)
      }
      fork {
        dut.io.id.readQueue.enqueue(rdq)
      }
      waitAndExpect(dut, expData, rdq.rd)
    }
  }

  it should "perform a ld.fcn" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.fcn")
    scala.util.Random.setSeed(-7334395889145041688L)
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)

      val ijk = genIJK()
      val ijkBundle = genIJKinput(IJK = Some(ijk), pad=false, mod=FCN)
      val rdq = genReadQueueBundle(reg=2, rf=XREG, iter=ijk(3), mod=FCN)
      val expData = calculateExpectedData(getFcnIndices(ijk), ijkBundle.baseAddr)
      fork {
        dut.io.id.neighbour.enqueue(ijkBundle)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq(rdq, rdq))
      }
      waitAndExpect(dut, expData, rdq.rd)
    }
  }

  it should "perform a ld.edn1" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.edn1")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)

      val ijk = genIJK()
      val ijkBundle = genIJKinput(IJK = Some(ijk), pad=false, mod=EDN1)
      val rdq = genReadQueueBundle(reg=3, rf=XREG, iter=ijk(3), mod=EDN1)
      val expData = calculateExpectedData(getEdn1Indices(ijk), ijkBundle.baseAddr)
      fork {
        dut.io.id.neighbour.enqueue(ijkBundle)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq(rdq, rdq))
      }
      waitAndExpect(dut, expData, rdq.rd)
    }
  }

  it should "perform a ld.edn2" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.edn2")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)

      val ijk = genIJK()
      val ijkBundle = genIJKinput(IJK = Some(ijk), pad=false, mod=EDN2)
      val rdq = genReadQueueBundle(reg=3, rf=XREG, iter=ijk(3), mod=EDN2)
      val expData = calculateExpectedData(getEdn2Indices(ijk), ijkBundle.baseAddr)
      fork {
        dut.io.id.neighbour.enqueue(ijkBundle)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq(rdq, rdq))
      }
      waitAndExpect(dut, expData, rdq.rd)
    }
  }

  it should "perform ld.fcn, ld.edn1, ld.edn2 and ld.sel in a row" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage neighbour loads")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(50)

      //Same ijk value is used for all loads
      val ijk = genIJK()
      val baseAddress = randomElement(baseAddresses)
      val fcn = genIJKinput(IJK = Some(ijk), baseAddress = Some(baseAddress), pad=false, mod=FCN)
      val edn1 = genIJKinput(IJK = Some(ijk), baseAddress = Some(baseAddress), pad=false, mod=EDN1)
      val edn2 = genIJKinput(IJK = Some(ijk), baseAddress = Some(baseAddress), pad=false, mod=EDN2)
      val sel = genIJKinput(IJK = Some(ijk), baseAddress = Some(baseAddress), pad=false, mod=SEL)

      val rdFcn = Seq.fill(2)(genReadQueueBundle(reg=0, rf=XREG, iter=ijk(3), mod=FCN))
      val rdEdn1 = Seq.fill(2)(genReadQueueBundle(reg=1, rf=XREG, iter=ijk(3), mod=EDN1))
      val rdEdn2 = Seq.fill(2)(genReadQueueBundle(reg=2, rf=XREG, iter=ijk(3), mod=EDN2))
      val rdSel = Seq(genReadQueueBundle(reg=3, rf=XREG, iter=ijk(3), mod=SEL))

      val expFcn = calculateExpectedData(getFcnIndices(ijk), baseAddress)
      val expEdn1 = calculateExpectedData(getEdn1Indices(ijk), baseAddress)
      val expEdn2 = calculateExpectedData(getEdn2Indices(ijk), baseAddress)
      val expSel = calculateExpectedData(Seq(elementIndex(ijk)), baseAddress)

      fork {
        dut.io.id.neighbour.enqueueSeq(Seq(fcn, edn1, edn2, sel))
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq.concat(rdFcn, rdEdn1, rdEdn2, rdSel))
      }
      waitAndExpect(dut, expFcn, rdFcn(0).rd)
      waitAndExpect(dut, expEdn1, rdEdn1(0).rd)
      waitAndExpect(dut, expEdn2, rdEdn2(0).rd)
      waitAndExpect(dut, expSel, rdSel(0).rd)
    }
  }

  it should "perform ld.dof followed by ld.elem" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.dof followed by ld.elem")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(25)
      //Need to generate XREG_DEPTH ijk-values.
      //Also need to generate a DOF operation for each of these + ELEM operations for all of these
      //Will need to poke XREG_DEPTH*3 entries to read queue when performing .dof, and XREG_DEPTH entries when performing .elem
      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)))
      val baseAddress = randomElement(baseAddresses)

      val dof = ijk.map(a => genIJKinput(IJK = Some(a),  Some(baseAddress), pad=false, mod=DOF))
      val elem = ijk.map(a => genIJKinput(IJK = Some(a), Some(baseAddress), pad=false, mod=ELEM))

      val rdDof = dof.zipWithIndex.flatMap(d => Seq.fill(3)(genReadQueueBundle(d._2, rf = VREG, iter = ijk(d._2)(3), mod = DOF)))
      val rdElem = Seq.fill(XREG_DEPTH)(genReadQueueBundle(reg=0, rf=XREG, iter=0, mod=ELEM))

      //For each element's DOF's, calculate expected data
      val expDof = getEdof(dof).map(e => calculateExpectedData(e, baseAddress))
      val expElem = calculateExpectedData(ijk.map(elementIndex), baseAddr = baseAddress)

      fork {
        dut.io.id.edof.enqueueSeq(dof)
        dut.io.id.neighbour.enqueueSeq(elem)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq(rdDof, rdElem).flatten)
      }
      (expDof, takeNth(rdDof, 3)).zipped.foreach((e, r) => waitAndExpect(dut, e, r.rd))
      waitAndExpect(dut, expElem, rdElem(0).rd)
    }
  }

  it should "perform ld.elem followed by ld.dof" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.elem followed by ld.dof")
    scala.util.Random.setSeed(1)
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)

      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)))
      val baseAddress = randomElement(baseAddresses)

      val dof = ijk.map(a => genIJKinput(IJK = Some(a),  Some(baseAddress), pad=false, mod=DOF))
      val elem = ijk.map(a => genIJKinput(IJK = Some(a), Some(baseAddress), pad=false, mod=ELEM))

      val rdDof = dof.zipWithIndex.flatMap(d => Seq.fill(3)(genReadQueueBundle(d._2, rf = VREG, iter = ijk(d._2)(3), mod = DOF)))
      val rdElem = Seq.fill(XREG_DEPTH)(genReadQueueBundle(reg=0, rf=XREG, iter=0, mod=ELEM))

      val expDof = getEdof(dof).map(e => calculateExpectedData(e, baseAddress))
      val expElem = calculateExpectedData(ijk.map(elementIndex), baseAddr = baseAddress)

      fork {
        dut.io.id.neighbour.enqueueSeq(elem)
        dut.io.id.edof.enqueueSeq(dof)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(Seq(rdElem, rdDof).flatten)
      }
      waitAndExpect(dut, expElem, rdElem(0).rd)
      (expDof, takeNth(rdDof, 3)).zipped.foreach((e, r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "returns all 0's on ld.dof when pad is asserted" in {
    simulationConfig(true)
    initMemory()
    seed("Memory stage ld.dof with padding")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(30)

      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)))
      val baseAddress = randomElement(baseAddresses)
      val dof = ijk.map(a => genIJKinput(IJK = Some(a),  Some(baseAddress), pad=true, mod=DOF))
      val rdDof = dof.zipWithIndex.flatMap(d => Seq.fill(3)(genReadQueueBundle(d._2, rf = VREG, iter = ijk(d._2)(3), mod = DOF)))
      val expDof = Seq.fill(VREG_SLOT_WIDTH)(Seq.fill(VREG_DEPTH)(0L))

      //Poke ijk data and readqueue data at the same time
      fork {
        dut.io.id.edof.enqueueSeq(dof)
      }
      fork {
        dut.io.id.readQueue.enqueueSeq(rdDof)
      }
      (expDof, takeNth(rdDof, 3)).zipped.foreach((e, r) => waitAndExpect(dut, e, r.rd))
    }
  }


  //Tests removed since this never should be the case, that we perform ld.vec and ld.dof in the same instruction
//
//  it should "perform ld.dof followed by ld.vec" in {
//    simulationConfig(true)
//    initMemory()
//    seed("Memory stage ld.dof and ld.vec")
//    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
//      dut.clock.setTimeout(20)
//      performLdDof(dut, pad=false)
//      performLdVec(dut)
//    }
//  }
//
//  it should "perform ld.vec followed by ld.dof" in {
//    simulationConfig(true)
//    initMemory()
//    seed("Memory stage ld.dof and ld.vec")
//    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
//      dut.clock.setTimeout(20)
//      performLdVec(dut)
//      performLdDof(dut, pad=false)
//    }
//  }
}

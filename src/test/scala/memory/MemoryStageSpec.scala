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
import execution.{RegisterBundle, RegisterFileType, StypeBaseAddress, StypeLoadStore, StypeMod, seed}
import execution.StypeMod._
import execution.StypeBaseAddress._
import execution.RegisterFileType._
import utils.DefaultMemInit

class MemoryStageSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Memory stage"

  val wordsPerBank = WORDS_PER_BANK
  val memInitFileLocation = "src/resources/meminit_default/"
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
   * Pokes data onto the write queue. Waits until the write queue 'ready' signal is high, and then pokes the given data
   * onto the queue, asserting valid for 1 clock cycle.
   * @param wq The write queue to poke data onto
   * @param wrData The data to be written
   * @param clock The clock of the DUT
   * @param mod The S-type modifier of the instruction being processed
   */
  def pokeWriteQueue(wq: DecoupledIO[WriteQueueBundle], clock: Clock, wrData: Seq[Long], mod: StypeMod.Type, iter: Int): Unit = {
    require(wrData.length <= 8, "Cannot poke than more 8 pieces of data onto the write queue at the same time")
    while(!wq.ready.peek.litToBoolean) {
      clock.step()
    }
    for(i <- wrData.indices) {
      wq.bits.wrData(i).poke(wrData(i).S)
    }
    wq.bits.mod.poke(mod)
    wq.bits.iter.poke(iter.U)
    timescope {
      wq.valid.poke(true.B)
      clock.step()
    }
  }

  /**
   * Enqueues an instruction onto the vector port of the memory stage. Waits until the 'ready' signal is high, then pokes the given data
   * onto the port, asserting valid for 1 clock cycle
   * @param vec The vec input port of the memory stage
   * @param clock The clock of the DUT
   * @param indices The indices to read/write from
   * @param baseAddr The base address of the read/write operation
   */
  def enqueueVec(vec: DecoupledIO[AddressGenProducerIO], clock: Clock, indices: Seq[Long], baseAddr: StypeBaseAddress.Type): Unit = {
    require(indices.length == 8, "Must poke 8 indices onto vector port")
    while(!vec.ready.peek.litToBoolean) {
      clock.step()
    }
    for(i <- indices.indices) {
      vec.bits.indices(i).poke(indices(i).U)
      vec.bits.validIndices(i).poke(true.B)
    }
    vec.bits.baseAddr.poke(baseAddr)
    timescope {
      vec.valid.poke(true.B)
      clock.step()
    }
  }

  /**
   * Pokes the VEC inputs of the memory stage.
   * @param dut The DUT
   * @param indices The indices to poke. If indices.length < NUM_MEMORY_BANKS, the remaining indices are set false
   * @param baseAddr The base address to load from.
   */
  def pokeVec(dut: MemoryStage, indices: Seq[Int], baseAddr: StypeBaseAddress.Type = StypeBaseAddress.X): Unit = {
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

  /**
   * Performs some generic setup on the ready/valid interfaces of the memory stage, to make later processing easier
   * @param dut
   */
  def setupClock(dut: MemoryStage): Unit = {
    dut.io.id.vec.initSource().setSourceClock(dut.clock)
    dut.io.id.readQueue.initSource().setSourceClock(dut.clock)
    dut.io.id.neighbour.initSource().setSourceClock(dut.clock)
    dut.io.id.edof.initSource().setSourceClock(dut.clock)
    dut.io.id.writeQueue.initSource().setSourceClock(dut.clock)
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

//  val annos = Seq.empty[firrtl.annotations.Annotation]
  val annos = Seq(WriteVcdAnnotation)

  it should "perform a ld.vec" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage ld.vec")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)

      val baseAddress = randomElement(baseAddresses)
      val readIndices = Seq.tabulate(ELEMS_PER_VSLOT/NUM_MEMORY_BANKS)(n => Seq.range[Long](n*NUM_MEMORY_BANKS, (n+1)*NUM_MEMORY_BANKS))
      val rdq = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.fill(3)(genReadQueueBundle(reg=n, rf=VREG, iter=0, mod=VEC))).flatten //iteration value is don'tcare in this scenario
      val expectIndices = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range(n*VREG_DEPTH, (n+1)*VREG_DEPTH))
      val expData = expectIndices.map(e => calculateExpectedData(e, baseAddress))

      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        for(ri <- readIndices) {
          enqueueVec(dut.io.id.vec, dut.clock, ri, baseAddress)
        }
      } .fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      (expData, takeNth(rdq, 3)).zipped.foreach((e,r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "perform a st.vec" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage st.dof")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(30)
      val baseAddress = randomElement(baseAddresses)
      val indices = Seq.tabulate(ELEMS_PER_VSLOT/NUM_MEMORY_BANKS)(n => Seq.range[Long](n*NUM_MEMORY_BANKS, (n+1)*NUM_MEMORY_BANKS))
      val wrData = indices
      val expData = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.range[Long](n*VREG_DEPTH, (n+1)*VREG_DEPTH))
      val rdq = Seq.tabulate(VREG_SLOT_WIDTH)(n => Seq.fill(3)(genReadQueueBundle(reg=n, rf=VREG, iter=0, mod=VEC))).flatten //iteration value is don'tcare in this scenario

      //Poke data
      dut.io.id.ls.poke(StypeLoadStore.STORE)
      fork {
        for(i <- indices.indices) {
          enqueueVec(dut.io.id.vec, dut.clock, indices(i), baseAddress)
        }
      } .fork {
        for(wd <- wrData) {
          pokeWriteQueue(dut.io.id.writeQueue, dut.clock, wd, VEC, iter=0)
        }
      }.join
      while(dut.io.ctrl.wqCount.peek().litValue() > 0) {
        dut.clock.step()
      }

      //Read dagta
      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        for(i <- indices.indices) {
          enqueueVec(dut.io.id.vec, dut.clock, indices(i), baseAddress)
        }
      } .fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      (expData, takeNth(rdq, 3)).zipped.foreach((e,r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "zero out remaining bits in ld.vec" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
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
    DefaultMemInit()
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

  it should "perform a st.dof" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage st.dof")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(10)
      val baseAddress = randomElement(baseAddresses)

      val ijk = genIJKmultiple()
      val instrs = ijk.map(e => genIJKinput(IJK=Some(e), pad=false, mod=DOF, baseAddress = Some(baseAddress)))
      val rdq = Seq.tabulate(XREG_DEPTH)(n => Seq.fill(3)(genReadQueueBundle(reg=n, rf=VREG, iter=ijk(n)(3), mod=DOF))).flatten
      val wrData = Seq.tabulate(ELEMS_PER_VSLOT/NUM_MEMORY_BANKS)(n => Seq.range[Long](n*NUM_MEMORY_BANKS, (n+1)*NUM_MEMORY_BANKS))
      val expData = Seq.tabulate(XREG_DEPTH)(n => Seq.range[Long](n*VREG_DEPTH, (n+1)*VREG_DEPTH))

      //Poke data onto mdodule
      dut.io.id.ls.poke(StypeLoadStore.STORE)
      fork {
        dut.io.id.edof.enqueueSeq(instrs)
      } .fork {
        for(wd <- wrData) {
          pokeWriteQueue(dut.io.id.writeQueue, dut.clock, wd, mod=DOF, iter=0)
        }
      }.join
      while(dut.io.ctrl.wqCount.peek.litValue() > 0) {
        dut.clock.step()
      }

      //Read back values
      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        dut.io.id.edof.enqueueSeq(instrs)
      } .fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      (expData, takeNth(rdq, 3)).zipped.foreach((e, r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "perform a st.fdof" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage st.fdof")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)
      val baseAddress = randomElement(baseAddresses)

      val ijk = genIJKmultiple(start=Some(Array(0,4,2))) //Choosing these start coordinates will allow us to observe indices where DOF's are stored, and indices where they are not
      val wrInstrs = ijk.map(e => genIJKinput(IJK=Some(e), pad=false, mod=FDOF, baseAddress=Some(baseAddress)))
      val rdInstrs = ijk.map(e => genIJKinput(IJK=Some(e), pad=false, mod=DOF, baseAddress=Some(baseAddress)))
      val rdq = Seq.tabulate(XREG_DEPTH)(n => Seq.fill(3)(genReadQueueBundle(reg=n, rf=VREG, iter=ijk(n)(3), mod=DOF))).flatten
      val edof = getEdof(wrInstrs)
      val expData = edof.map(e => calculateExpectedData(e, baseAddress).toArray)
      //Storing all 1s for simplicity's sake
      val wrData = Seq.fill(ELEMS_PER_VSLOT/NUM_MEMORY_BANKS)(Seq.fill(NUM_MEMORY_BANKS)(1L))
      //Iterate through expData and ijk together. If ijk(0) == 0, set expData 0-3, 8-11 and 16-19 to 1's, otherwise keep intact
      for(j <- expData.indices) {
        val e = expData(j)
        val i = ijk(j)
        if(i(0) == 0) {
          for(k <- 0 until 3) {
            for(l <- 0 until 4) {
              e(k*NUM_MEMORY_BANKS+l) = 1
            }
          }
        }
      }
      //Write
      dut.io.id.ls.poke(StypeLoadStore.STORE)
      fork {
        dut.io.id.edof.enqueueSeq(wrInstrs)
      } .fork {
        for(wd <- wrData) {
          pokeWriteQueue(dut.io.id.writeQueue, dut.clock, wd, FDOF, 0)
        }
      }.join
      while(dut.io.ctrl.wqCount.peek.litValue() > 0) {
        dut.clock.step()
      }
      //read
      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        dut.io.id.edof.enqueueSeq(rdInstrs)
      } .fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      (expData, takeNth(rdq, 3)).zipped.foreach((e,r) => waitAndExpect(dut, e, r.rd))
    }
  }

  it should "perform a ld.elem" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage ld.elem")
    //Try generating multiple IJK values
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(20)
      //Generate ijk-values and their corresponding iteration numbers. Poke this onto the DUT in a linear fashion
      val ijkVals = genIJKmultiple()
      val ijk = ijkVals.map(a => genIJKinput(Some(a), Some(StypeBaseAddress.X), pad=false, mod=ELEM))
      //Indices accessed are based on iteration values
      val rdq = ijkVals.map(a => genReadQueueBundle(reg=0, rf=XREG, iter=a(3), mod=ELEM))
      val expData = calculateExpectedData(ijkVals.map(elementIndex), baseAddr = X)

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

  it should "perform a st.elem" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage st.elem")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) { dut =>
      setupClock(dut)
      dut.clock.setTimeout(30)
      val ijk = genIJKmultiple()
      val baseAddr = randomElement(baseAddresses)
      val instrs = ijk.map(e => genIJKinput(IJK = Some(e), pad=false, mod=ELEM, baseAddress = Some(baseAddr)))
      val rdq = ijk.map(e => genReadQueueBundle(reg=0, rf=XREG, iter=e(3), mod=ELEM))
      val wrData = Seq.range[Long](0, XREG_DEPTH)
      val expData = wrData


      //Poke data onto queue
      dut.io.id.ls.poke(StypeLoadStore.STORE)
      fork {
        dut.io.id.neighbour.enqueueSeq(instrs)
      } .fork {
        for(i <- wrData.indices) { //Only the data written at index (0) is actually used
          pokeWriteQueue(wq=dut.io.id.writeQueue, clock=dut.clock, Seq(wrData(i)), mod=ELEM, iter=ijk(i)(3))
        }
      }.join
      while(dut.io.ctrl.wqCount.peek.litValue() > 0) {
        dut.clock.step()
      }
      //Read data from memory
      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        dut.io.id.neighbour.enqueueSeq(instrs)
      } .fork {
        dut.io.id.readQueue.enqueueSeq(rdq)
      }
      waitAndExpect(dut, expData, rdq(0).rd)
    }
  }

  it should "perform a ld.sel" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
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

  it should "perform a st.sel" in {
    //This test should first write a value to a location in memory, and then try to retrieve that same value
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
    seed("Memory stage st.sel")
    test(new MemoryStage(wordsPerBank, memInitFileLocation)).withAnnotations(annos) {dut =>
      setupClock(dut)
      dut.clock.setTimeout(10)

      val ijk = genIJK()
      val instr = genIJKinput(IJK=Some(ijk), pad=false, mod=SEL)
      val rdq = genReadQueueBundle(reg=1, rf=XREG, iter=ijk(3), mod=SEL)
      val expData = Seq(1L)
      //Poking data onto write queue requires manual work, since vec literals don't work yet :(
      dut.io.id.ls.poke(StypeLoadStore.STORE)
      fork {
        dut.io.id.neighbour.enqueue(instr)
      } .fork {
        pokeWriteQueue(wq=dut.io.id.writeQueue, clock=dut.clock, wrData=Seq(1), mod=SEL, iter=ijk(3))
      }.join
      while(dut.io.ctrl.wqCount.peek.litValue > 0) {
        dut.clock.step()
      }
      dut.io.id.ls.poke(StypeLoadStore.LOAD)
      fork {
        dut.io.id.neighbour.enqueue(instr)
      } .fork {
        dut.io.id.readQueue.enqueue(rdq)
      }
      waitAndExpect(dut, expData, rdq.rd)
    }
  }

  it should "perform a ld.fcn" in {
    simulationConfig(true)
    initMemory()
    DefaultMemInit()
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
    DefaultMemInit()
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
    DefaultMemInit()
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
    DefaultMemInit()
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
    DefaultMemInit()
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
    DefaultMemInit()
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
    DefaultMemInit()
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
}

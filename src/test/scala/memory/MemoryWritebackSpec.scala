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
import pipeline.{RegisterBundle, RegisterFileType, StypeBaseAddress, StypeMod}
import pipeline.StypeMod._
import pipeline.seed


class MemoryWritebackSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory writeback stage"

  /**
   * Pokes the rdData-ports of the MemoryWriteback module in order of increasing index.
   * If data.length < rdData.size, the remaining ports will keep their values from the previous iteration
   * @param dut The DUT
   * @param data The data to poke onto the DUT
   */
  def pokeData(dut: MemoryWriteback, data: Seq[Double]): Unit = {
    for(i <- data.indices) {
      dut.io.mem.bits.rdData(i).poke(double2fixed(data(i)).S)
    }
  }

  /**
   * Expects output data on the ID-side of the memory writeback module, checking  the indices in increasing order.
   * If data.length < rdData.size, the remaining ports will not be checked.
   * @param dut The DUT
   * @param data The data to expect
   */
  def expectData(dut: MemoryWriteback, data: Seq[Double]): Unit = {
    for(i <- data.indices) {
      dut.io.id.wrData(i).expect(double2fixed(data(i)).S)
    }
    dut.io.id.we.expect(true.B)
  }

  /**
   * Pokes the read queue input port of the memory writeback stage
   * @param dut The DUT
   * @param iter The iteration number associated with the transaction
   * @param mod The S-type modifier associated with the transacction
   * @param rd The destination register for the load operation
   */
  def pokeReadQueue(dut: MemoryWriteback, iter: Int, mod: StypeMod.Type, rd: RegisterBundle): Unit = {
    dut.io.readQueue.bits.rd.poke(rd)
    dut.io.readQueue.bits.iteration.poke(iter.U)
    dut.io.readQueue.bits.mod.poke(mod)
  }

  /**
   * Performs all of the logic necessary for driving and checking an entire VEC or DOF transaction
   * @param dut The DUT
   * @param mod The modifier to use. Only valid types are DOF and VEC
   */
  def transactionVecDof(dut: MemoryWriteback, mod: StypeMod.Type): Unit = {
    require(mod == DOF || mod == VEC, "This method should only be called with Stype modifiers DOF or VEC")
    //Apply increasing values. Expect to see the output on the third cycle
    //All input rd,mod values should be the same. Iteration is a don't-care in this case
    val data1 = Array.fill(NUM_MEMORY_BANKS)(genDouble())
    val data2 = Array.fill(NUM_MEMORY_BANKS)(genDouble())
    val data3 = Array.fill(NUM_MEMORY_BANKS)(genDouble())
    val rd = (new RegisterBundle).Lit(_.reg -> 0.U, _.rf -> RegisterFileType.VREG, _.subvec -> 0.U, _.rfUint -> 0.U)

    pokeData(dut, data1)
    pokeReadQueue(dut, 0, VEC, rd)
    dut.io.mem.valid.poke(true.B)
    dut.clock.step()

    dut.io.id.we.expect(false.B)
    pokeData(dut, data2)
    dut.clock.step()

    pokeData(dut, data3)
    dut.io.id.we.expect(false.B)
    dut.clock.step()

    expectData(dut, Array.concat(data1, data2, data3))
    dut.io.id.rd.expect(rd)
  }

  /**
   * Performs all of the logic necessary for driving and checking an entire elem transaction
   * @param dut The DUT
   */
  def transactionElem(dut: MemoryWriteback): Unit = {
    //Random rdData values and iteration values. The rd value is kept constant
    val data = Seq.fill(XREG_DEPTH)(genDouble())
    val iters = Seq.fill(XREG_DEPTH)(scala.util.Random.nextInt(NUM_MEMORY_BANKS))
    val rd = (new RegisterBundle).Lit(_.reg -> 2.U, _.rf -> RegisterFileType.XREG, _.subvec -> 0.U, _.rfUint -> 0.U)

    dut.io.mem.valid.poke(true.B)
    for(i <- data.indices) {
      pokeData(dut, Seq.fill(8)(0)) //Clear all inputs, set our input to another value
      dut.io.mem.bits.rdData(iters(i)).poke(double2fixed(data(i)).S)
      pokeReadQueue(dut, iters(i), ELEM, rd)
      if(i != 0) {
        dut.io.id.we.expect(false.B)
      }
      dut.clock.step()
    }
    expectData(dut, data)
    dut.io.id.rd.expect(rd)
  }

  /**
   * Performs all logic necessary for driving and checking a ld.sel transaction
   * @param dut The DUT
   */
  def transactionSel(dut: MemoryWriteback): Unit = {
    val data = genDouble()
    val iter = scala.util.Random.nextInt(NUM_MEMORY_BANKS)
    val rd = (new RegisterBundle).Lit(_.reg -> 3.U, _.rf -> RegisterFileType.XREG, _.subvec -> 0.U, _.rfUint -> 0.U)

    dut.io.mem.valid.poke(true.B)
    pokeData(dut, Seq.fill(8)(0))
    dut.io.mem.bits.rdData(iter).poke(double2fixed(data).S)
    pokeReadQueue(dut, iter, SEL, rd)
    dut.clock.step()

    expectData(dut, Seq(data))
    dut.io.id.rd.expect(rd)
  }

  /**
   * Given an iteration value, returns the sequence of indices that the face neighbours occupy
   * @param iter The iteration number [0;7]
   * @return A Seq containing the 3 indices at which face neighbours are located
   */
  def getFcnIndices(iter: Int): Seq[Int] = {
    require(0 <= iter && iter <= 7, "Iteration must be between 0 and 7 inclusive")
    iter match {
      case 0 => Seq(1, 2, 4)
      case 1 => Seq(0, 3, 5)
      case 2 => Seq(0, 3, 6)
      case 3 => Seq(1, 2, 7)
      case 4 => Seq(0, 5, 6)
      case 5 => Seq(1, 4, 7)
      case 6 => Seq(2, 4, 7)
      case 7 => Seq(3, 5, 6)
      case _ => throw new IllegalArgumentException("Iteration must be between 0 and 7 inclusive")
    }
  }

  /**
   * Given an iteration value, returns the sequence of indices that the edge neighbours occupy
   * @param iter The iteration number [0;7]
   * @return A seq containing the 3 indices at which edge neighbours are located
   */
  def getEdnIndices(iter: Int): Seq[Int] = {
    require(0 <= iter && iter <= 7, "Iteration must be between 0 and 7 inclusive")
    iter match {
      case 0 => Seq(3, 5, 6)
      case 1 => Seq(2, 4, 7)
      case 2 => Seq(1, 4, 7)
      case 3 => Seq(0, 5, 6)
      case 4 => Seq(1, 2, 7)
      case 5 => Seq(0, 3, 6)
      case 6 => Seq(0, 3, 5)
      case 7 => Seq(1, 2, 4)
      case _ => throw new IllegalArgumentException("Iteration must be between 0 and 7 inclusive")
    }
  }

  /**
   * Performs all of the logic necessary for driving a checking a ld.fcn, ld.edn1 or ld.edn2 transaction
   * @param dut The DUT
   * @param mod The modifier to use. Valid values are FCN, EDN1 and EDN2
   */
  def transactionNeighbours(dut: MemoryWriteback, mod: StypeMod.Type): Unit = {
    val data1 = Seq.fill(3)(genDouble())
    val data2 = Seq.fill(3)(genDouble())
    val iter = scala.util.Random.nextInt(NUM_MEMORY_BANKS)
    val rdReg = mod match {
      case FCN => 4
      case EDN1 => 5
      case EDN2 => 6
      case _ => throw new IllegalArgumentException("Neighbour transactions are only valid with S-type mods FCN, EDN1 or EDN2")
    }
    val rd = (new RegisterBundle).Lit(_.reg -> rdReg.U, _.rf -> RegisterFileType.XREG, _.subvec -> 0.U, _.rfUint -> 0.U)
    val ind = if(mod == FCN) getFcnIndices(iter) else getEdnIndices(iter)

    //Poke first round of data
    dut.io.mem.valid.poke(true.B)
    pokeData(dut, Seq.fill(8)(0))
    pokeReadQueue(dut, iter, mod, rd)
    for(i <- ind.indices) {
      dut.io.mem.bits.rdData(ind(i)).poke(double2fixed(data1(i)).S)
    }
    dut.clock.step()

    //Poke second round of data
    pokeData(dut, Seq.fill(8)(0))
    pokeReadQueue(dut, iter, mod, rd)
    for(i <- ind.indices) {
      dut.io.mem.bits.rdData(ind(i)).poke(double2fixed(data2(i)).S)
    }
    dut.clock.step()

    val res = Seq.concat(data1, data2)
    expectData(dut, res)
    dut.io.id.rd.expect(rd)
  }

  "Memory writeback stage" should "build a VEC result" in {
    seed("Memory writeback VEC")
    test(new MemoryWriteback) {dut =>
      transactionVecDof(dut, VEC)
    }
  }

  it should "build a DOF result" in {
    seed("Memory writeback DOF")
    test(new MemoryWriteback) {dut =>
      transactionVecDof(dut, DOF)
    }
  }

  it should "build an ELEM result" in {
    seed("Memory writeback ELEM")
    test(new MemoryWriteback).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      transactionElem(dut)
    }
  }

  it should "build a SEL result" in {
    seed("Memory writeback SEL")
    test(new MemoryWriteback) {dut =>
      transactionSel(dut)
    }
  }

  it should "build a FCN result" in {
    seed("Memory writeback FCN")
    test(new MemoryWriteback) {dut =>
      transactionNeighbours(dut, FCN)
    }
  }

  it should "build a EDN1 result" in {
    seed("Memory writeback EDN1")
    test(new MemoryWriteback) {dut =>
      transactionNeighbours(dut, EDN1)
    }
  }

  it should "build a EDN2 result" in {
    seed("Memory writeback EDN2")
    test(new MemoryWriteback) {dut =>
      transactionNeighbours(dut, EDN2)
    }
  }

  it should "build an ELEM followed by DOF" in {
    seed("Memory writeback ELEM and DOF")
    test(new MemoryWriteback).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      transactionElem(dut)
      transactionVecDof(dut, DOF)
    }
  }

  it should "build a VEC followed by an ELEM" in {
    seed("Memory writeback VEC and ELEM")
    test(new MemoryWriteback) {dut =>
      transactionVecDof(dut, VEC)
      transactionElem(dut)
    }
  }

  it should "build face, edges and single" in {
    seed("Memory writeback face, edges and single")
    scala.util.Random.setSeed(1)
    test(new MemoryWriteback).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      transactionNeighbours(dut, FCN)
      transactionNeighbours(dut, EDN1)
      transactionNeighbours(dut, EDN2)
      transactionSel(dut)
    }
  }
}

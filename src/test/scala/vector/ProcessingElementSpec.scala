package vector

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
import ProcElemOpcode._

class ProcessingElementSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Processing elements"

  /**
   * Tests the behaviour of a processing elements by applying stimuli and observing the results.
   * This method should only be used to test a stream of inputs without breaks
   * @param dut The DUT
   * @param as First operands in the instruction
   * @param bs Second operands in the instruction
   * @param results Expected results
   * @param ops Operations performed
   */
  def testStreamBehaviour(dut: ProcessingElement,
                          as: Array[Long],
                          bs: Array[Long],
                          results: Array[Long],
                          ops: Array[UInt]): Unit = {
    var i = 0
    var resultCnt = 0
    val itermax = results.length
    while(resultCnt < itermax && i < 500) {
      if(i < itermax) {
        dut.io.in.a.poke(as(i).S)
        dut.io.in.b.poke(bs(i).S)
        dut.io.in.op.poke(ops(i))
        dut.io.in.valid.poke(true.B)
      } else {
        dut.io.in.valid.poke(false.B)
      }
      dut.clock.step()
      if(dut.io.out.valid.peek().litToBoolean) {
        //Using assert instead of expect due to rounding errors when dividing.
        assert(math.abs(fixed2double(results(resultCnt)) - sint2double(dut.io.out.res.peek)) < 1E-4)
        resultCnt += 1
      }
      i += 1
    }
  }

  def generateStimuliSingleOperation(dut: ProcessingElement, op: UInt, iters: Int): Unit = {
    val as = new Array[Long](iters)
    val bs = new Array[Long](iters)
    val results = new Array[Long](iters)
    val ops = new Array[UInt](iters)

    for (i <- 0 until iters ) {

      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      val res = op match {
        case ADD => fixedAdd(a, b)
        case SUB => fixedSub(a, b)
        case MUL => fixedMul(a, b)
        case DIV => double2fixed(x / y)
        case _ => throw new IllegalArgumentException("Unsupported PE Operation")
      }
      as(i) = a
      bs(i) = b
      results(i) = res
      ops(i) = op
    }
    testStreamBehaviour(dut, as, bs, results, ops)
  }

  def generateStimuliAddSub(dut: ProcessingElement, iters: Int): Unit = {
    val as = new Array[Long](iters)
    val bs = new Array[Long](iters)
    val results = new Array[Long](iters)
    val ops = new Array[UInt](iters)

    for (i <- 0 until iters ) {

      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      val op = if( scala.util.Random.nextBoolean() ) {ADD} else {SUB}
      val res = op match {
        case ADD => fixedAdd(a, b)
        case SUB => fixedSub(a, b)
        case _ => throw new IllegalArgumentException("Unsupported PE Operation")
      }
      as(i) = a
      bs(i) = b
      results(i) = res
      ops(i) = op
    }
    testStreamBehaviour(dut, as, bs, results, ops)
  }

  def testMacRandom(dut: ProcessingElement, iters: Int): Unit = {
    //Generate stimuli
    val as = new Array[Long](iters)
    val bs = new Array[Long](iters)
    var result = 0L
    val op = MAC

    //We need to scale down the values so they don't explode out of our range
    for (i <- 0 until iters ) {
      val x = getDouble()*math.pow(2,-(INT_WIDTH-2))
      val y = getDouble()*math.pow(2,-(INT_WIDTH-2))
      val a = double2fixed(x)
      val b = double2fixed(y)
      result += fixedMul(a,b)
      as(i) = a
      bs(i) = b
    }

    //Pokey pokey
    var i = 0
    while(!dut.io.out.valid.peek.litToBoolean && i < 50) {
      if(i < iters) {
        dut.io.in.a.poke(as(i).S)
        dut.io.in.b.poke(bs(i).S)
        dut.io.in.valid.poke(true.B)
        dut.io.in.op.poke(op)
        dut.io.in.macLimit.poke(iters.U)
      }
      dut.clock.step()
      i += 1
    }
    dut.io.out.res.expect(result.S)
  }

  def testMacMultiple(dut: ProcessingElement, iters: Int, macLimit: Int): Unit = {
    //Generate stimuli
    val as = Array.ofDim[Long](iters, macLimit)
    val bs = Array.ofDim[Long](iters, macLimit)
    var results = new Array[Long](iters)
    var result = 0L
    val op = MAC

    //We need to scale down the values so they don't explode out of our range
    for(i <- 0 until iters) {
      for (j <- 0 until macLimit ) {
        val x = getDouble()*math.pow(2,-(INT_WIDTH-2))
        val y = getDouble()*math.pow(2,-(INT_WIDTH-2))
        val a = double2fixed(x)
        val b = double2fixed(y)
        result += fixedMul(a,b)
        as(i)(j) = a
        bs(i)(j) = b
      }
      results(i) = result
      result = 0
    }
    //Pokey pokey
    var i = 0
    var resultCnt = 0
    while(resultCnt < iters && i < 100) {
      val iter = i/macLimit //Which MAC iteration are we on?
      val inp = i % macLimit //Which input vector are we on
      if(i < iters*macLimit) {

        dut.io.in.a.poke(as(iter)(inp).S)
        dut.io.in.b.poke(bs(iter)(inp).S)
        dut.io.in.valid.poke(true.B)
        dut.io.in.op.poke(op)
        dut.io.in.macLimit.poke(macLimit.U)
      }
      dut.clock.step()
      i += 1
      if(dut.io.out.valid.peek.litToBoolean) {
        dut.io.out.res.expect(results(resultCnt).S)
        resultCnt += 1
      }
    }
//    dut.io.out.res.expect(result.S)
  }


  private val iters = 50

  it should "add a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, ADD, iters)
    }
  }

  it should "subtract a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, SUB, iters)
    }
  }

  it should "multiply a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, MUL, iters)
    }
  }

  it should "divide a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, DIV, iters)
    }
  }

  it should "handle a mixture of adds and subs" in {
    test(new ProcessingElement) {c =>
      generateStimuliAddSub(c, iters)
    }
  }

  it should "handle a randomly generated mac instruction" in {
    test(new ProcessingElement) {c =>
      testMacRandom(c, 20)
    }
  }

  it should "handle sequential mac instructions" in {
    test(new ProcessingElement) {c =>
      testMacMultiple(c, 10, 10)
    }
  }

  //TODO: Check that we can deassert MAC and/or enable during calculations, and the result will stay in resReg
  //Once reasserted, it should keep calculating
  it should "stop calculations when MAC is desasserted" in {

  }

  //We should be able to finish a MAC and immediatedly after execute another operation
  it should "transition smoothly from MAC to all other operations" in {

  }

  //Setting macLimit to another value once started is ok, but it shouldn't latch in until finished with the current accumulate
  //the new maclimit should be used for the next round of MAC's
  it should "not allow us to change maclimit once started" in {

  }
}

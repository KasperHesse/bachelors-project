package vector

import chisel3._
import chisel3.util._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Fixed._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation
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
        dut.io.in.en.poke(true.B)
      } else {
        dut.io.in.en.poke(false.B)
      }
      dut.clock.step()
      if(dut.io.out.done.peek().litToBoolean) {
        //Using assert instead of expect due to rounding errors when dividing.
        assert(math.abs(fixed2double(results(resultCnt)) - sint2double(dut.io.out.res.peek)) < 1E-5)
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
        case ProcessingElement.ADD => fixedAdd(a, b)
        case ProcessingElement.SUB => fixedSub(a, b)
        case ProcessingElement.MUL => fixedMul(a, b)
        case ProcessingElement.DIV => double2fixed(x / y)
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
      val op = if( scala.util.Random.nextBoolean() ) {ProcessingElement.ADD} else {ProcessingElement.SUB}
      val res = op match {
        case ProcessingElement.ADD => fixedAdd(a, b)
        case ProcessingElement.SUB => fixedSub(a, b)
        case _ => throw new IllegalArgumentException("Unsupported PE Operation")
      }
      as(i) = a
      bs(i) = b
      results(i) = res
      ops(i) = op
    }
    testStreamBehaviour(dut, as, bs, results, ops)
  }


  private val iters = 50
  it should "add a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, ProcessingElement.ADD, iters)
    }
  }

  it should "subtract a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, ProcessingElement.SUB, iters)
    }
  }

  it should "multiply a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, ProcessingElement.MUL, iters)
    }
  }

  it should "divide a stream of numbers" in {
    test(new ProcessingElement) {c =>
      generateStimuliSingleOperation(c, ProcessingElement.DIV, iters)
    }
  }

  it should "handle a mixture of adds and subs" in {
    test(new ProcessingElement) {c =>
      generateStimuliAddSub(c, iters)
    }
  }

//  it should "handle a multiply-accumulate instruction" in {
//
//  }
}

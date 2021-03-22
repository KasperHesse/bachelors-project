package pipeline

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import utils.Config._
import utils.Fixed._
import vector.Opcode
import chisel3.experimental.BundleLiterals._
import chiseltest.experimental.TestOptionBuilder._
import chiseltest.internal.WriteVcdAnnotation

class ExecuteSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Execution stage"


  /**
   * Generates one set of stimuli for applying to the Execute stage.
   * @param op The opcode to use for the operation
   * @param nelem The number of processing elements
   * @return An 2D-array containing in (0) the a-inputs, in (1) the b-inputs and in (2) the results of a OP b
   *         Each array is nelem long.
   */
  def genStimuli(op: Opcode.Type, nelem: Int): Array[Array[Long]] = {
    val as = Array.ofDim[Long](nelem)
    val bs = Array.ofDim[Long](nelem)
    val results = Array.ofDim[Long](nelem)

    for (i <- 0 until nelem) {
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      as(i) = a
      bs(i) = b
      results(i) = op match {
        case Opcode.ADD => fixedAdd(a, b)
        case Opcode.SUB => fixedSub(a, b)
        case Opcode.MUL => fixedMul(a, b)
        case Opcode.DIV => double2fixed(x / y)
        case _ => throw new IllegalArgumentException("Opcode unknown")
      }
    }
    Array(as, bs, results)
  }
  /**
   * Tests the Execute stage by applying two input vectors and observing the outputs.
 *
   * @param dut The DUT
   * @param op Opcode. See [[vector.Opcode]]
   */
  def testSubVector(dut: Execute, op: Opcode.Type): Unit = {
    val nelem = NUM_PROCELEM
    val stim = genStimuli(op, nelem)
    val as = stim(0)
    val bs = stim(1)
    val results = stim(2)

    var i = 0
    var resultCnt = 0
    for(j <- 0 until nelem) {
      dut.io.in.a(j).poke(as(j).S)
      dut.io.in.b(j).poke(bs(j).S)
    }
    dut.io.in.op.poke(op)
    val dest = (new Destination).Lit(_.rd -> 3.U, _.subvec -> 1.U)
    dut.io.in.dest.poke(dest)
    dut.io.ctrl.stall.poke(false.B) //Accept new inputs
    dut.clock.step()
    dut.io.ctrl.stall.poke(true.B) //Don't accept them anymore
    dut.io.in.op.poke(Opcode.NOP)
    dut.clock.step()
    while(i < 200 && resultCnt < 1) {
      dut.io.ctrl.count.expect(1.U)
      i += 1
      if(dut.io.out.valid.peek.litToBoolean) {
        for(j <- 0 until nelem) {
          assert(math.abs(fixed2double(results(j)) - fixed2double(dut.io.out.res(j).peek)) < 1E-4)
        }
        resultCnt += 1
        dut.io.out.dest.expect(dest)
      }
      dut.clock.step()
    }
    dut.io.ctrl.count.expect(0.U)
    assert(resultCnt == 1)
  }

  /**
   * Tests the execute stage by applying multiple subvectors in succession, observing the outputs
   * @param dut The DUT
   * @param op The operation to perform
   * @param count The number of subvectors to generate and apply
   */
  def testMultiVector(dut: Execute, op: Opcode.Type, count: Int): Unit = {
    //Generate stimuli
    val nelem = NUM_PROCELEM
    val numSubVectors = VECTOR_REGISTER_DEPTH/NUM_PROCELEM
    val as = Array.ofDim[Long](count, nelem)
    val bs = Array.ofDim[Long](count, nelem)
    val results = Array.ofDim[Long](count, nelem)
    val destinations = Array.ofDim[Destination](count)

    for(i<- 0 until count) {
      val stim = genStimuli(op, nelem)
      as(i) = stim(0)
      bs(i) = stim(1)
      results(i) = stim(2)
      destinations(i) = (new Destination).Lit(_.rd -> i.U, _.subvec -> (i % numSubVectors).U)
    }

    var i = 0
    var resultCnt = 0
    dut.io.in.op.poke(op)
    dut.io.ctrl.stall.poke(false.B)
    while(i < 200 && resultCnt < count) {
      if(i < count) {
        for(j <- 0 until nelem) {
          dut.io.in.a(j).poke(as(i)(j).S)
          dut.io.in.b(j).poke(bs(i)(j).S)
        }
        dut.io.in.dest.poke(destinations(i))
      } else {
        dut.io.ctrl.stall.poke(true.B)
      }
      i += 1

      if(dut.io.out.valid.peek.litToBoolean) {
        for(j <- 0 until nelem) {
          assert(math.abs(fixed2double(results(resultCnt)(j)) - fixed2double(dut.io.out.res(j).peek)) < 1E-4)
        }
        dut.io.out.dest.expect(destinations(resultCnt))
        resultCnt += 1
      }
      dut.clock.step()
    }
    dut.io.ctrl.count.expect(0.U)
    assert(resultCnt == count)
  }

  /**
   * Tests whether the system properly handles MAC operations
   * @param dut the DUT
   * @param macLimit the number of multiply-accumulates that should be performed before the result is output
   */
  def testMACOperation(dut: Execute, macLimit: Int): Unit = {
    //Generate input vectors
    val nelem = NUM_PROCELEM
    val as = Array.ofDim[Long](macLimit, nelem)
    val bs = Array.ofDim[Long](macLimit, nelem)
    val tempResults = Array.ofDim[Long](macLimit, nelem) //intermediate results
    val results = Array.ofDim[Long](nelem)
    val dest = (new Destination).Lit(_.rd -> 1.U, _.subvec -> 2.U)

    for (i <- 0 until macLimit) {
      val stim = genStimuli(Opcode.MUL, nelem)
      as(i) = stim(0)
      bs(i) = stim(1)
      tempResults(i) = stim(2)
    }
    //Actual, final result
    for (i <- 0 until macLimit) {
      for (j <- 0 until nelem) {
        results(j) = fixedAdd(results(j), tempResults(i)(j))
      }
    }
    var i = 0
    var resultCnt = 0
    //Input poke
    dut.io.in.dest.poke(dest)
    dut.io.in.macLimit.poke(macLimit.U)
    dut.io.in.op.poke(Opcode.MAC)
    dut.io.ctrl.stall.poke(false.B)
    while (i < 200 && resultCnt < 1) {
      if (i < macLimit) {
        for (j <- 0 until nelem) {
          dut.io.in.a(j).poke(as(i)(j).S)
          dut.io.in.b(j).poke(bs(i)(j).S)
        }
      } else {
        dut.io.ctrl.stall.poke(true.B)
      }
      i += 1
      if (dut.io.out.valid.peek.litToBoolean) {
        for (j <- 0 until nelem) {
          assert(math.abs(fixed2double(results(j)) - fixed2double(dut.io.out.res(j).peek)) < 1E-4)
        }
        dut.io.out.dest.expect(dest)
        resultCnt += 1
      }
      dut.clock.step()
    }
    assert(resultCnt == 1)
    dut.io.ctrl.count.expect(0.U)
  }

  def testMACMultiple(dut: Execute, macLimit: Int, iters: Int): Unit = {
      //Generate input vectors
      val nelem = NUM_PROCELEM
      val as = Array.ofDim[Long](iters, macLimit, nelem)
      val bs = Array.ofDim[Long](iters, macLimit, nelem)
      val tempResults = Array.ofDim[Long](iters, macLimit, nelem) //intermediate results
      val results = Array.ofDim[Long](iters, nelem)
      val dest = Array.ofDim[Destination](iters)

      for(i <- 0 until iters) {
        for (j <- 0 until macLimit) {
          val stim = genStimuli(Opcode.MUL, nelem)
          as(i)(j) = stim(0)
          bs(i)(j) = stim(1)
          tempResults(i)(j) = stim(2)
        }
        dest(i) = (new Destination).Lit(_.rd -> ((i+1) % 32).U, _.subvec -> ((i+1) % 4).U)
      }
    //Actual, final result
    for(i <- 0 until iters) {
      for (j <- 0 until macLimit) {
        for (k <- 0 until nelem) {
          results(i)(k) = fixedAdd(results(i)(k), tempResults(i)(j)(k))
        }
      }
    }
    //TODO Finish this test
    var i = 0
    var iter = 0
    var resultCnt = 0
    dut.io.ctrl.stall.poke(false.B)
    while(i < 200 && resultCnt < iters) {
      iter = i/iters
      if(iter < iters) {
        dut.io.in.dest.poke(dest(iter))
        dut.io.in.macLimit.poke(macLimit.U)
        dut.io.in.op.poke(Opcode.MAC)
        val s = i % macLimit
        for(j <- 0 until nelem) {
          dut.io.in.a(j).poke(as(iter)(s)(j).S)
          dut.io.in.b(j).poke(bs(iter)(s)(j).S)
        }
      } else {
        dut.io.ctrl.stall.poke(true.B)
      }
      i += 1
      if(dut.io.out.valid.peek.litToBoolean) {
        for(j <- 0 until nelem) {
          assert(math.abs(fixed2double(results(resultCnt)(j)) - fixed2double(dut.io.out.res(j).peek)) < 1E-4)
        }
        dut.io.out.dest.expect(dest(resultCnt))
        resultCnt += 1
      }
      dut.clock.step()
    }
    assert(resultCnt == iters)
    dut.io.ctrl.count.expect(0.U)
  }

  it should "perform vector-vector addition of two subvectors" in {
    test(new Execute){c =>
      testSubVector(c, Opcode.ADD)
    }
  }

  it should "perform vector-vector subtraction of two subvectors" in {
    test(new Execute) {c =>
      testSubVector(c, Opcode.SUB)
    }
  }

  it should "perform vector-vector multiplication of two subvectors" in {
    test(new Execute) {c =>
      testSubVector(c, Opcode.MUL)
    }
  }

  it should "perform vector-vector division of two subvectors" in {
    test(new Execute) {c =>
      testSubVector(c, Opcode.DIV)
    }
  }

  //Multiple: Generate a whole load of input vectors (preferably 20+), and shoot them in one after another
  it should "perform multiple additions in a row" in {
    test(new Execute) {c =>
      testMultiVector(c, Opcode.ADD, 5)
    }
  }

  it should "perform multiple subtractions in a row" in {
    test(new Execute) {c =>
      testMultiVector(c, Opcode.SUB, 5)
    }
  }

  it should "perform multiple multiplications in a row" in {
    test(new Execute) {c =>
      testMultiVector(c, Opcode.MUL, 5)
    }
  }

  it should "perform multiple divisions in a row" in {
    test(new Execute) {c =>
      testMultiVector(c, Opcode.DIV, 32)
    }
  }

  //Assert mac, macLimit on first clock cycle, assert inputs on all cycles afterwards
  it should "perform a MAC operation" in {
    test(new Execute).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      testMACOperation(c, 5)
    }
  }

  it should "perform multiple MAC operations in a row" in {
    test(new Execute).withAnnotations(Seq(WriteVcdAnnotation)) {c =>
      testMACMultiple(c, 5, 5)
    }
  }
}

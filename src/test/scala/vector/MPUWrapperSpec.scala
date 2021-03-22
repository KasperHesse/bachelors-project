package vector

import chisel3._
import chiseltest._
import chiseltest.internal.WriteVcdAnnotation
import utils.Fixed._
import org.scalatest.{FlatSpec, Matchers}
import chiseltest.experimental.TestOptionBuilder._
import utils.Config._
import vector.Opcode.MAC

class MPUWrapperSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "MPU Wrapper"

  /**
   * Tests the MPU wrapper by applying two input vectors and observing the outputs
 *
   * @param dut     The DUT
   * @param as      Vector of a inputs. Should be [[VECTOR_REGISTER_DEPTH]] long
   * @param bs      Vector of b inputs. Should be [[VECTOR_REGISTER_DEPTH]] long
   * @param results Vector of expected results. Should be [[VECTOR_REGISTER_DEPTH]] long
   * @param op      Opcode. Only elementwise opcodes should be used with this test
   * @param count   The number of elements in (as, bs, results) that are valid. All other outputs are expected 0
   */
  def testVector(dut: MPUWrapper, as: Array[Long], bs: Array[Long], results: Array[Long], op: MPUopcode.Type, count: Int): Unit = {
    //Pokey pokey
    var i = 0
    var resultCnt = 0
    val nelem = dut.nelem
    for(j <- 0 until as.length) {
      dut.io.in.a(j).poke(as(j).S)
      dut.io.in.b(j).poke(bs(j).S)
    }
    dut.io.in.vectorLength.poke(count.U)
    dut.io.in.op.poke(op)
    dut.io.in.rd.poke(3.U)
    dut.io.in.stall.poke(false.B)
    dut.clock.step()
    dut.io.in.stall.poke(true.B)
    while(i < 200 && resultCnt < count) {
      dut.clock.step()
      i += 1
      if(dut.io.out.valid.peek.litToBoolean) {
        for(j <- 0 until nelem) {
//          dut.io.out.res(j).expect(results(resultCnt).S)
          assert(math.abs(fixed2double(results(resultCnt)) - fixed2double(dut.io.out.res(j).peek)) < 1E-4)
          resultCnt += 1
        }
        dut.io.out.rd.expect(3.U)
      }
    }
  }

  def generateStimuli(dut: MPUWrapper, op: MPUopcode.Type, count: Int) : Unit = {
    //Generate stimuli
    val as = Array.ofDim[Long](VECTOR_REGISTER_DEPTH)
    val bs = Array.ofDim[Long](VECTOR_REGISTER_DEPTH)
    val results = Array.ofDim[Long](VECTOR_REGISTER_DEPTH)

    //We need to scale down the values so they don't explode out of our range
    for (i <- 0 until VECTOR_REGISTER_DEPTH) {
//      val x = getDouble()*math.pow(2,-(INT_WIDTH-2))
//      val y = getDouble()*math.pow(2,-(INT_WIDTH-2))
      val x = getDouble()
      val y = getDouble()
      val a = double2fixed(x)
      val b = double2fixed(y)
      as(i) = a
      bs(i) = b
      if(i < count) {
        results(i) = op match {
          case MPUopcode.ADDV => fixedAdd(a, b)
          case MPUopcode.SUBV => fixedSub(a, b)
          case MPUopcode.MULV => fixedSub(a, b)
          case MPUopcode.DIVV => double2fixed(x / y)
          case _ => throw new IllegalArgumentException("Opcode unknown")
        }
      } else {
        results(i) = 0L
      }
    }
    testVector(dut, as, bs, results, op, count)
  }


  //In the below, "full vector" = VECTOR_REGISTER_WIDTH elements large
  //and "short vector" < VECTOR_REGISTER_WIDTH element large
  it should "perform vector-vector addition on two full vectors" in {
    test(new MPUWrapper(NUM_PROCELEM)) {c =>
      generateStimuli(c, MPUopcode.ADDV, VECTOR_REGISTER_DEPTH)
    }
  }

  it should "perform vector-vector addition of two short vectors" in {
    test(new MPUWrapper(NUM_PROCELEM)) {c =>
      generateStimuli(c, MPUopcode.ADDV, 15)
    }
  }

  it should "perform vector-vector subtraction of two long vectors" in {
    test(new MPUWrapper(NUM_PROCELEM)) {c =>
      generateStimuli(c, MPUopcode.SUBV, VECTOR_REGISTER_DEPTH)
    }
  }

  it should "perform vector-vector subtraction of two short vectors" in {
    test(new MPUWrapper(NUM_PROCELEM)) {c =>
      generateStimuli(c, MPUopcode.SUBV, 24)
    }
  }

  it should "perform vector-vector multiplication of two long vectors" in {

  }

  it should "perform vector-vector multiplication of two short vectors" in {

  }

  it should "perform vector-vector division of two long vectors" in {

  }

  it should "perform vector-vector division of two short vectors" in {

  }

  it should "perform dot product of two long vectors" in {

  }

  it should "perform dot product of two short vectors" in {

  }

  it should "perform dot product on two vectors larger than bus width " in {

  }

  it should "perform matrix-vector product / mac on a vector" in {

  }
}

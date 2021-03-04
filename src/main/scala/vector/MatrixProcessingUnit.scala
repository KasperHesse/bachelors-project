package vector

import chisel3._
import utils.Fixed.FIXED_WIDTH

/**
 * A matrix processing unit, utilizing several [[ProcessingElement]] to perform matrix/vector/scalar operations.
 * Implements [[MPUIO]]
 * @param nelem The number of processing elements to be instantiated
 */
class MatrixProcessingUnit(nelem: Int) extends Module {
  val io = IO(new MPUIO(nelem))

  val PE = for(i <- 0 until nelem) yield {
    Module(new ProcessingElement)
  }

  //Set up multiplexers to select between same element from B moving onto all PE's, or separate elements from B

  //Connections to A-modules and enable signals
  for(i <- 0 until nelem) {
    PE(i).io.in.a := io.in.a(i)
    PE(i).io.in.b := io.in.b(i)
    PE(i).io.in.op := io.in.op
    PE(i).io.in.en := io.in.en
    io.out.res(i) := PE(i).io.out.res
  }

  val dones = for(i <- 0 until nelem) yield {
    PE(i).io.out.done
  }
  io.out.done := dones.reduce( (a, b) => a && b)
}

/**
 * I/O ports for a matrix processing unit.
 * @param nelem The number of processing elements to be instantiated / number of ports for a,b operands
 */
class MPUIO(val nelem: Int) extends Bundle {
  val in = Input(new MPUInput(nelem))
  val out = Output(new MPUOutput(nelem))


  /**
   * Input ports for a matrix processing unit.
   * @param nelem Number of processing elements to be instantiated / number of operands per operation
   */
  class MPUInput(val nelem: Int) extends Bundle {
    /** Vector of first operands */
    val a = Vec(nelem, SInt(FIXED_WIDTH.W))
    /** Vector of second operands */
    val b = Vec(nelem, SInt(FIXED_WIDTH.W))
    /** Opcode. See [[ProcessingElement]] */
    val op = UInt(5.W)
    /** Enable signal. Asserted for one clock cycle when input operands are valid
     * Alternative solution: Use a register to check if op was previously 0 and is now non-zero. If this is the case,
     * assert enable on all PE's*/
    val en = Bool()

//    override def cloneType = (new MPUInput(nelem)).asInstanceOf[this.type]
  }

  /**
   * Output ports for a matrix processing unit.
   * @param nelem Number of processing elements to be instantiated / number of operands per operation
   */
  class MPUOutput(val nelem: Int) extends Bundle {
    /** Vector of results produced by the processing elements */
    val res = Vec(nelem, SInt(FIXED_WIDTH.W))
    /** Asserted when the outputs are valid */
    val done = Bool()
  }
}



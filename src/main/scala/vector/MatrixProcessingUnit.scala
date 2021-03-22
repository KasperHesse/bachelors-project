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
    PE(i).io.in.valid := io.in.valid && !(io.in.op === Opcode.NOP)
    PE(i).io.in.macLimit := io.in.macLimit
    io.out.res(i) := PE(i).io.out.res
  }

  val valids = for(i <- 0 until nelem) yield {
    PE(i).io.out.valid
  }
  io.out.valid := valids.reduce((a, b) => a && b) //And reduction
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
    val op = Opcode()
    /** Data valid signal. Asserted for one clock cycle when input operands are valid */
    val valid = Bool()
    /** Number of values to add before returning a result.  */
    val macLimit = UInt(32.W)
  }

  /**
   * Output ports for a matrix processing unit.
   * @param nelem Number of processing elements to be instantiated / number of operands per operation
   */
  class MPUOutput(val nelem: Int) extends Bundle {
    /** Vector of results produced by the processing elements */
    val res = Vec(nelem, SInt(FIXED_WIDTH.W))
    /** Asserted for one clock cycle when outputs are valid */
    val valid = Bool()
  }
}



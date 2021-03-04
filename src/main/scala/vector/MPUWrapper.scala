package vector

import chisel3._

/**
 * A wrapper around the matrix processing unit, holding the MPU as well the hardcoded memory block(s) holding the
 * KE-matrix used for a large number of operations.
 * This is also where multiplexers are used to select between sending the same element from the B vector to all PE's
 * in the MPU, or sending `nelem` separate values at once
 * @param nelem The number of processing elements to instantiate / operands to use
 */
class MPUWrapper(val nelem: Int) extends Module {
  val io = IO(new MPUWrapIO)
}

class MPUWrapIO extends Bundle {

}

package utils

import chisel3.SInt

/**
 * A general placeholder class, used for defining all constants used throughout the workflow
 */
object Config {
  //Configuration values
  /** What type of divisor to use if nothing is explicitly stated */
  val DIVTYPE = DivTypes.NEWTONRAPHSON
  /** What type of multiplier to use if nothing is explicitly stated when instantiating */
  val MULTYPE = MulTypes.SINGLECYCLE
  /** How many stages to use in Newton-Raphson divisors stage 3 */
  val NRDIV_STAGE3_REPS = 3

  //Compile-time constant definitions
  /** Number of element in the x-direction */
  val NELX = 3
  /** Number of elements in the y-direction */
  val NELY = 3
  /** Number of element in the z-direction */
  val NELZ = 3
  /** The greatest of the three element dimensions */
  val GDIM = math.max(NELX, math.max(NELY, NELZ))

  /** Number of nodes in the x-direction */
  val NX = NELX + 1
  /** Number of nodes in the y-direction */
  val NY = NELY + 1
  /** Number of nodes in the z-direction */
  val NZ = NELZ + 1

  /** Total number of element DOF in the design domain */
  val NDOF = 3*NX*NY*NZ

  val WORD_LENGTH = Fixed.FIXED_WIDTH

  /** The number of element in the vector register file */
  var NUM_VECTOR_REGISTERS = 32
  /** The number of values stored in each entry in the vector register file */
  var VECTOR_REGISTER_DEPTH = 24
  /** The number of elements in the scalar register file */
  var NUM_SCALAR_REGISTERS = 32
  /** The number of processing elements used in the design. Determines the width of vectors carrying values between modules. */
  var NUM_PROCELEM = 8
  /** The number of subvectors in each vector register */
//  val NUM_SUBVECTORS = VECTOR_REGISTER_DEPTH/NUM_PROCELEM
  /** The number of vector register slots that are adressible from instructions */
  var NUM_VREG_SLOTS = 4
  /** The width (in registers) of each vector register slot */
//  val VREG_SLOT_WIDTH = NUM_VECTOR_REGISTERS/NUM_VREG_SLOTS

  /** Simulation flag. Assert inside of a tester to use simulation-specific functionality */
  var SIMULATION = false

  require(NUM_VECTOR_REGISTERS >= NUM_VREG_SLOTS, "Must have more vector registers than register slots")
  require(NUM_VECTOR_REGISTERS % NUM_VREG_SLOTS == 0, "Number of vector registers must me a multiple of register slots")

  require(NRDIV_STAGE3_REPS >= 1, "Newton-Raphson division requires at least one iteration in stage 3")
}

object DivTypes extends Enumeration {
  type DivisorType = Value
  val NEWTONRAPHSON, SRT = Value
}

object MulTypes extends Enumeration {
  type MulType = Value
  val SINGLECYCLE, MULTICYCLE = Value
}

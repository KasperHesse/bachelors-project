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
  var NRDIV_STAGE3_REPS = 3

  //Compile-time constant definitions
  /** Number of element in the x-direction */
  val NELX = 6
  /** Number of elements in the y-direction */
  val NELY = 6
  /** Number of element in the z-direction */
  val NELZ = 6
  /** The greatest of the three element dimensions */
  var GDIM = math.max(NELX, math.max(NELY, NELZ))

  /** Number of nodes in the x-direction */
  val NX = NELX + 1
  /** Number of nodes in the y-direction */
  val NY = NELY + 1
  /** Number of nodes in the z-direction */
  val NZ = NELZ + 1

  /** Total number of elements in the design domain */
  var NELEM = NELX * NELY * NELZ
  /** Total number of element DOF in the design domain */
  var NDOF = 3 * NX * NY * NZ

  /** The number of element in the vector register file */
  var NUM_VREG = 24
  /** The number of vector register slots that are adressible from instructions */
  var NUM_VREG_SLOTS = 4
  /** The number of values stored in each entry in the vector register file */
  var VREG_DEPTH = 24
  /** The number of X-value registers in the x register file. */
  var NUM_XREG = 8
  /** The number of values stored in each entry in the x-register file */
  var XREG_DEPTH = NUM_VREG/NUM_VREG_SLOTS
  /** The number of elements in the scalar register file */
  var NUM_SREG = 16
  /** The number of processing elements used in the design. Determines the width of vectors carrying values between modules. */
  var NUM_PROCELEM = 6
  /** The number of subvectors in each vector register */
  var SUBVECTORS_PER_VREG = VREG_DEPTH/NUM_PROCELEM
  /** The width (number of registers) of each vector register slot */
  var VREG_SLOT_WIDTH = NUM_VREG/NUM_VREG_SLOTS
  /** The width and height of the KE matrix */
  var KE_SIZE = 24
  /** The bitwidth of an instruction */
  var INSTRUCTION_WIDTH = 32
  /** The total number of elements stored in a vector register slot
   * This value is the increment amount used when processing vectors elementwise */
  var ELEMS_PER_VSLOT = VREG_SLOT_WIDTH*VREG_DEPTH
  /** Number of elements parsed when performing an operation over NELEM-long vectors */
  var NELEMLENGTH = if(NELEM % ELEMS_PER_VSLOT == 0) NELEM else (NELEM/ELEMS_PER_VSLOT+1)*ELEMS_PER_VSLOT
  /** Number of elements parsed when performing an operation over NDOF-long vectors */
  var NDOFLENGTH = if(NDOF % ELEMS_PER_VSLOT == 0) NDOF else (NDOF/ELEMS_PER_VSLOT+1)*ELEMS_PER_VSLOT

  /** The number of memory banks used / the number of values that can be loaded from memory at once */
  var NUM_MEMORY_BANKS = 8
  /** Width of memory addresses */
  var MEM_ADDR_WIDTH = 32

  /** Simulation flag. Assert inside of a tester to use simulation-specific functionality */
  var SIMULATION = false

  checkRequirements()

  /** Checks if all configurations requirements are held. This *must* be called in a tester if config values are changed */
  def checkRequirements(): Unit = {
    require(NUM_VREG >= NUM_VREG_SLOTS, "Must have more vector registers than register slots")
    require(NUM_VREG % NUM_VREG_SLOTS == 0, "Number of vector registers must me a multiple of vector register slots")
    require(KE_SIZE == VREG_DEPTH, s"KE_SIZE must equal VREG_DEPTH for proper matrix-vector products. They are $KE_SIZE, $VREG_DEPTH")
    require(NRDIV_STAGE3_REPS > 0, "Newton-Raphson division requires at least one iteration in stage 3")
    require(XREG_DEPTH == VREG_SLOT_WIDTH, s"XREG_DEPTH and VREG_SLOT_WIDTH must be equal. Got $XREG_DEPTH and $VREG_SLOT_WIDTH")
    require(VREG_SLOT_WIDTH == NUM_VREG/NUM_VREG_SLOTS, s"VREG_SLOT_WIDTH must equal NUM_VREG/NUM_VREG_SLOTS. Got $VREG_SLOT_WIDTH, should be ${NUM_VREG/NUM_VREG_SLOTS}")
    require(SUBVECTORS_PER_VREG == VREG_DEPTH/NUM_PROCELEM, s"SUBVECTORS_PER_VREG must equal VREG_DPETH/NUM_PROCELEM. Got $SUBVECTORS_PER_VREG, should be ${VREG_DEPTH/NUM_PROCELEM}")
    require(NUM_PROCELEM == VREG_SLOT_WIDTH, "NUM_PROCELEM and VREG_SLOT_WIDTH must be the same. VREG_SLOT_WIDTH is a calculated property")
    require(ELEMS_PER_VSLOT == VREG_SLOT_WIDTH*VREG_DEPTH, "ELEMS_PER_VSLOT must equal VREG_SLOT_WIDTH*VREG_DEPTH. ELEMS_PER_SLOT is a calculated property")
    require(NELEM > ELEMS_PER_VSLOT, s"NELEM(${NELEM}) must be greater than ELEMS_PER_VSLOT(${ELEMS_PER_VSLOT}) for proper computation")
    require(NDOF > NELEM, "NDOF must be greater than NELEM")
    require(NUM_SREG <= 16, "NUM_SREG must be <= 16 as only 4 bits are used for register fields")

    NELEMLENGTH = if(NELEM % ELEMS_PER_VSLOT == 0) NELEM else (NELEM/ELEMS_PER_VSLOT+1)*ELEMS_PER_VSLOT
    NDOFLENGTH = if(NDOF % ELEMS_PER_VSLOT == 0) NDOF else (NDOF/ELEMS_PER_VSLOT+1)*ELEMS_PER_VSLOT
  }
}

object DivTypes extends Enumeration {
  type DivisorType = Value
  val NEWTONRAPHSON, SRT = Value
}

object MulTypes extends Enumeration {
  type MulType = Value
  val SINGLECYCLE, MULTICYCLE = Value
}

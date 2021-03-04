package utils

import chisel3.SInt

/**
 * A general placeholder class, used for defining all constants used throughout the workflow
 */
object Config {
  val DIVTYPE = DivTypes.NEWTONRAPHSON
  val MULTYPE = MulTypes.SINGLECYCLE
  val NRDIV_STAGE3_REPS = 3


  if(NRDIV_STAGE3_REPS < 1) {
    throw new IllegalArgumentException("Newton-Raphson division requires at least one iteration in stage 3")
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

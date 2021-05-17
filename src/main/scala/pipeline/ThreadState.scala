package pipeline

import chisel3.experimental.ChiselEnum

object ThreadState extends ChiselEnum {
  val sIdle, sLoad, sEstart, sExec, sEend, sStore, sEnd = Value
  val sWait1, sWait2 = Value
}

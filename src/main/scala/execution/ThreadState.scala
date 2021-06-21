package execution

import chisel3.experimental.ChiselEnum

object ThreadState extends ChiselEnum {
  val sIdle, sLoad, sEstart, sExec, sEend, sStore, sPend = Value
  val sWait1, sWait2 = Value
}

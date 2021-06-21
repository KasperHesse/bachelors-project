package execution

import chisel3.experimental.ChiselEnum

object DecodeState extends ChiselEnum {
  val sIdle, sLoad, sExec, sBranch, sBranch2 = Value
}

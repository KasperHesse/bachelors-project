package pipeline

import chisel3.experimental.ChiselEnum

object DecodeState extends ChiselEnum {
  val sIdle, sLoad, sExec, sFinalize = Value
}

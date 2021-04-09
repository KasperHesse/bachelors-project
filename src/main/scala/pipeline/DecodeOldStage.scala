package pipeline

import chisel3.experimental.ChiselEnum

object DecodeOldStage extends ChiselEnum {
  val sIdle, sLoad, sExecute = Value
}
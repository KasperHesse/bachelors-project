package pipeline

import chisel3.experimental.ChiselEnum

object DecodeStage extends ChiselEnum {
  val sIdle, sLoad, sExecute = Value
}
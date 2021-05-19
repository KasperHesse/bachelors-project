package memory

import chisel3._
import chisel3.util.Decoupled

class MemWbIO extends Bundle {
  val mem = Flipped(Decoupled(new MemoryWritebackIO))
  val id = Output(new pipeline.WbIdIO)
}

class MemoryWriteback extends Module {
  val io = IO(new Bundle {})
}

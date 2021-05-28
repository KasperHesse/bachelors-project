package memory

import chisel3._
import chisel3.util.{Decoupled, RegEnable}

class VecWrapperIO extends Bundle {
  val in = Flipped(Decoupled(new AddressGenProducerIO))
  val out = Decoupled(new AddressGenProducerIO)
}

/**
 * A simple wrapper module around the 'vec' input to the memory stage. Serves as a pipeline register for the incoming
 * value before they are forwarded to the address generator. Implemented here to make [[MemoryStage]] as clutter-free as possible
 */
class VecWrapper extends Module {
  val io = IO(new VecWrapperIO)

  val reg = RegEnable(io.in.bits, io.out.ready)
  val valid = RegEnable(io.in.valid, io.out.ready)
  io.out.valid := valid
  io.out.bits := reg
  io.in.ready := io.out.ready
}

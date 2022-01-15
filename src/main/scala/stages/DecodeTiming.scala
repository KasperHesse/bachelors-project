package stages

import chisel3._
import execution._
import utils.TimingWrapper

/**
 * A module which encompasses the Decode stage and timing module. Used for testing tstart/tend instructions
 */
class DecodeTiming(val clkFreq: Int, val memfile: String) extends Module {
  val io = IO(new Bundle {
//    val fe = Flipped(new IfIdIO)
//    val fectrl = Flipped(new IfControlIO)
//    val idctrl = Flipped(new IdControlIO)
    val idout = new IdExIO
    val time = new TimingOutput(clkFreq)
  })

  val fetch = Module(new Fetch(memfile = memfile))
  val decode = Module(new Decode)
  val control = Module(new Control)
  val timing = Module(new TimingWrapper(clkFreq))

  fetch.io.id <> decode.io.fe
  fetch.io.ctrl <> control.io.fe
  decode.io.time <> timing.io.id
  decode.io.ctrl <> control.io.id
  decode.io.mem <> DontCare
  decode.io.memWb <> DontCare
  decode.io.wb <> DontCare
  io.idout <> decode.io.ex

  control.io.mem := DontCare
  control.io.ex := DontCare

  //Override some previously set dontcares
  control.io.mem.rqCount := 0.U
  control.io.mem.wqCount := 0.U

  control.io.ex.empty := true.B
  control.io.ex.macEmpty := true.B
  decode.io.mem.vec.ready := true.B
  decode.io.mem.writeQueue.ready := true.B

  io.time := timing.io.out
}

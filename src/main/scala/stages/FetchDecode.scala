package execution
import chisel3._

/**
 * A module containing only an instruction fetch + decode module
 * @param memfile
 */
class IfDec(memfile: String) extends Module {
  val io = IO(new IdExIO)

  val fetch = Module(new Fetch(memfile = memfile))
  val decode = Module(new Decode)
  val control = Module(new Control)

  fetch.io.id <> decode.io.fe
  decode.io.ex <> io

  decode.io.ctrl <> control.io.id
  fetch.io.ctrl <> control.io.fe

  decode.io.mem := DontCare
  decode.io.wb := DontCare
  control.io.ex := DontCare
}

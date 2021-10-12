package memory

import chisel3._
import chisel3.util.{Decoupled, log2Ceil, switch}
import execution.StypeMod._
import utils.Config.XREG_DEPTH

class WriteQueueWrapperIO extends Bundle {
  val in = Flipped(Decoupled(new WriteQueueBundle))
  val out = Decoupled(new WriteQueueBundle)
}

/**
 * A wrapper / pre-stage before the write queue.
 * When performing st.sel and st.elem operations, the stored data should be moved to the correct output index,
 * based on the ijk iteration value that spawned them. This module reorders write data to the correct ijk iteration value
 */
class WriteQueueWrapper extends Module {
  val io = IO(new WriteQueueWrapperIO)

  io.out <> io.in

  //ELEM operations require us to select the correct element from the incoming data. After any ELEM operation,
  //the counter should always have reset to 0
  val cnt = RegInit(0.U(log2Ceil(XREG_DEPTH+1).W))
  cnt := Mux(io.in.valid && io.in.bits.mod === ELEM, Mux(cnt === (XREG_DEPTH-1).U, 0.U, cnt + 1.U), cnt)
  when(io.in.bits.mod === SEL || io.in.bits.mod === ELEM) {
    io.out.bits.wrData(io.in.bits.iter) := io.in.bits.wrData(0)
  }
}

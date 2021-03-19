package control

import chisel3._
import chisel3.util._
import utils.Config._

/**
 * Takes the length of the current vector, count of how many elements of the vector are currently processed
 * and the difference between the two values, and uses this to output a one-hot code controlling the PE input multiplexers
 * between correct values and 0's.
 * If the selection bit is true, the a,b values should be used, otherwise zeros should be forwarded
 */
class SelectionBitsGenerator extends Module {
  val io = IO(new Bundle {
    val diff = Input(UInt(log2Ceil(VECTOR_REGISTER_DEPTH+1).W))
    val length = Input(UInt(log2Ceil(VECTOR_REGISTER_DEPTH+1).W))
    val processed = Input(UInt(log2Ceil(VECTOR_REGISTER_DEPTH+1).W))
    /** The one-hot coded output signal */
    val selectBits = Output(Vec(NUM_PROCELEM, Bool()))
  })

  val lowerBits = io.diff(log2Ceil(NUM_PROCELEM), 0)

  when(io.processed >= io.length) { //All remanining outputs should be zero
    io.selectBits := VecInit(Seq.fill(NUM_PROCELEM)(false.B))
  } .elsewhen (io.diff < NUM_PROCELEM.U) {
    io.selectBits := (UIntToOH(lowerBits, NUM_PROCELEM) - 1.U).asBools()
  } .otherwise {
    io.selectBits := VecInit(Seq.fill(NUM_PROCELEM)(true.B))
  }
}

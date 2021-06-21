package execution

import chisel3._
import utils.Config._
import chisel3.util._
import RegisterFileType._
import chisel3.experimental.BundleLiterals._

/**
 * This class is used to avoid execution hazard. It replaces the ordinary Queue implementation, since we need
 * to be able to view several of the first elements at once.
 */
class DestinationQueue extends Module {
  val NUM_ENTRIES = 32
  require(isPow2(NUM_ENTRIES), "Currently only works with a power-of-two number of entries")
  val io = IO(new Bundle {
    /** Whether to enqueue the current inputs */
    val enq = Input(Bool())
    /** Whether to dequeue an element from the EHA storage*/
    val deq = Input(Bool())
    /** Destination information to be stored */
    val destIn = Input(new RegisterBundle)
    /** The four destinations at the head of the queue */
    val head = Output(Vec(4, new ValidRegisterBundle))
    /** The destination at the very head of the queue */
    val destOut = Output(new RegisterBundle)
    /** Whether the queue is empty */
    val empty = Output(Bool())
  })

  val queue = RegInit(VecInit(Seq.fill(NUM_ENTRIES)((new RegisterBundle).Lit(_.rf -> VREG, _.reg -> 0.U, _.subvec -> 0.U))))
  val valids = RegInit(VecInit(Seq.fill(NUM_ENTRIES)(false.B)))

  val enq_ptr = RegInit(0.U(5.W))
  val deq_ptr = RegInit(0.U(5.W))

  when(io.enq) {
    queue(enq_ptr) := io.destIn
    valids(enq_ptr) := true.B
    enq_ptr := enq_ptr + 1.U
  }
  when(io.deq && valids(deq_ptr)) {
    valids(deq_ptr) := false.B
    deq_ptr := deq_ptr + 1.U
  }

  for(i <- 0 until 4) {
    io.head(i).dest := queue(deq_ptr + i.U)
    io.head(i).valid := valids(deq_ptr + i.U)
  }

  io.destOut := queue(deq_ptr)
  io.empty := enq_ptr === deq_ptr
}

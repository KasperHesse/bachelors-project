package test

import chisel3._
import chisel3.util._

class QueueTest extends Module {
  val io = IO(new Bundle{
    val ready = Input(Bool())
    val valid = Input(Bool())
    val din = Input(UInt(8.W))
    val dout = Output(UInt(8.W))

    val enq_ready = Output(Bool())
    val deq_valid = Output(Bool())
    val qcount = Output(UInt())
})

  val q = Module(new Queue(UInt(8.W), 4))
  q.io.enq.valid := io.valid
  q.io.deq.ready := io.ready
  io.enq_ready := q.io.enq.ready
  io.deq_valid := q.io.deq.valid
  io.dout := q.io.deq.bits
  q.io.enq.bits := io.din
  io.qcount := q.io.count
}

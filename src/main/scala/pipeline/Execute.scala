package pipeline

import chisel3._
import vector._
import utils.Config._
import vector.Opcode._

/**
 * The vector execution stage of the pipeline. Implements [[ExecuteIO]].
 * The execute stage must not be supplied with new inputs of differing types while processing long instructions.
 * To ensure this, a signal must be asserted which tells it to keep the current op (io.ctrl.stall).
 * When stall is asserted, the previously asserted op will be kept, but the internal 'valid' signal and 'newDest' signals will be forced false
 * @note On most operations, newDest should be true on every clock cycle. On MAC operations, newDest should only be true on the first clock cycle of the mac inputs
 */
class Execute extends Module {
  val io = IO(new ExecuteIO)

  val MPU = Module(new MatrixProcessingUnit(NUM_PROCELEM))
  val destinationQueue = Module(new util.Queue(new Destination(), 20)) //20 is a magic number right now. May not be enough?
  val in = RegNext(io.in)

  val op = RegInit(Opcode.NOP)
  val newDest = RegInit(false.B)
  //Stall / NOP overrides to ensure we don't process anything
  op := Mux(io.ctrl.stall || io.in.op === NOP, op, io.in.op)
  newDest := Mux(io.ctrl.stall || io.in.op === NOP, false.B, io.in.newDest)

  val validOp = !RegNext(io.ctrl.stall) && RegNext(io.in.op) =/= Opcode.NOP

  //Hard assignments
  for(i <- 0 until NUM_PROCELEM) {
    MPU.io.in.a(i) := in.a(i)
    MPU.io.in.b(i) := in.b(i)
  }
  MPU.io.in.valid := validOp
  MPU.io.in.op := op
  MPU.io.in.macLimit := in.macLimit

  destinationQueue.io.enq.bits := in.dest
  destinationQueue.io.enq.valid := newDest
  destinationQueue.io.deq.ready := MPU.io.out.valid

  io.out.res := MPU.io.out.res
  io.out.valid := MPU.io.out.valid
  io.out.dest := destinationQueue.io.deq.bits

  io.ctrl.count := destinationQueue.io.count
  io.ctrl.op := op
}

/**
 * I/O ports for the vector execution stage.
 */
class ExecuteIO extends Bundle {
  val in = Flipped(new IdExIO)
  val out = new ExWbIO
  val ctrl = new ExControlIO
}



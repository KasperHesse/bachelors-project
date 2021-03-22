package pipeline

import chisel3._
import vector._
import utils.Config._
import vector.Opcode._

/**
 * The vector execution stage of the pipeline. Implements [[ExecuteIO]].
 * The execute stage must not be supplied with new inputs of differing types while processing long instructions.
 * To this end, a signal must be asserted which tells it to keep the current op (io.ctrl.stall)
 * @note When executing multiple MAC-type operations, the 'dest' input must be kept constant for all inputs for the same MAC operation.
 *       Two subsequent MAC operations *must* have different 'dest' inputs - differing either in the destination register or the destination subvector
 */
class Execute extends Module {
  val io = IO(new ExecuteIO)

  val MPU = Module(new MatrixProcessingUnit(NUM_PROCELEM))
  val destinationQueue = Module(new util.Queue(new Destination(), 20)) //20 is a magic number right now. May not be enough?
  val in = RegNext(io.in)

  val op = RegInit(Opcode.NOP)
  op := Mux(io.ctrl.stall || io.in.op === NOP, op, io.in.op)

  val destPrev = RegNext(in.dest)
  val newDest = (destPrev.rd =/= in.dest.rd || destPrev.subvec =/= in.dest.subvec)

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
  destinationQueue.io.enq.valid := Mux(op === MAC, newDest & validOp, validOp)
  destinationQueue.io.deq.ready := MPU.io.out.valid

  io.out.res := MPU.io.out.res
  io.out.valid := MPU.io.out.valid
  io.out.dest := destinationQueue.io.deq.bits

  io.ctrl.count := destinationQueue.io.count
}

/**
 * I/O ports for the vector execution stage.
 */
class ExecuteIO extends Bundle {
  val in = Flipped(new IdExIO)
  val out = new ExWbIO
  val ctrl = new ExControlIO
}



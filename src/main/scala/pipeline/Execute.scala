package pipeline

import chisel3._
import vector._
import utils.Config._
import vector.Opcode._
import chisel3.util._

/**
 * I/O ports for the vector execution stage.
 */
class ExecuteIO extends Bundle {
  val in = Flipped(new IdExIO)
  val out = new ExWbIO
  val ctrl = new ExControlIO
  val fwd = new ExFwdIO
}

/**
 * The vector execution stage of the pipeline. Implements [[ExecuteIO]].
 * The execute stage must not be supplied with new inputs of differing types while processing long instructions.
 * To ensure this, a signal must be asserted which tells it to keep the current op (io.ctrl.stall).
 * When stall is asserted, the previously asserted op will be kept, but the internal 'valid' signal and 'newDest' signals will be forced false
 * Note that other inputs like a,b are allowed to flow into the execute stage, since a change in these values does not provoke a change in the system
 * @note On most operations, newDest should be true on every clock cycle. On MAC operations, newDest should only be true on the first clock cycle of the mac inputs
 */
class Execute extends Module {
  val io = IO(new ExecuteIO)

  // --- MODULES ---
  val MPU = Module(new MatrixProcessingUnit(NUM_PROCELEM))
  val macDestQueue = Module(new util.Queue(new RegisterBundle(),2))
  val destinationQueue = Module(new DestinationQueue())

  // --- REGISTERS ---
  val in = RegNext(io.in)
  val op = RegInit(Opcode.NOP)
  val newDest = RegInit(false.B)

  // --- LOGIC ---
  /* Valid signal going into the MPU. We need this separate from newDest since MAC operations require
   * a constant valid signal, but a non-constant newDest signal*/
  val validIn = !RegNext(io.ctrl.stall) && RegNext(io.in.op) =/= Opcode.NOP

  //Stall / NOP overrides to ensure we don't process anything
  val opSignal = Mux(io.ctrl.stall || io.in.op === NOP, op, io.in.op)
  op := opSignal
  val newDestSignal = Mux(io.ctrl.stall || io.in.op === NOP, false.B, io.in.newDest)
  newDest := newDestSignal

  // --- CONNECTIONS ---
  MPU.io.in.a := Mux(io.fwd.rs1swap, io.fwd.rs1newData, in.a)
  MPU.io.in.b := Mux(io.fwd.rs2swap, io.fwd.rs2newData, in.b)
  MPU.io.in.valid := validIn
  MPU.io.in.op := op
  MPU.io.in.macLimit := in.macLimit

  //We need a SEPARATE destination for MAC instructions, to allow other instructions to be processed at the same time
  destinationQueue.io.destIn := io.in.dest
  destinationQueue.io.enq := newDestSignal && (opSignal =/= MAC)

  macDestQueue.io.enq.bits := io.in.dest
  macDestQueue.io.enq.valid := newDestSignal && (opSignal === MAC)

  //Output stuff
  destinationQueue.io.deq := MPU.io.out.valid && !MPU.io.out.macResult
  macDestQueue.io.deq.ready := MPU.io.out.valid && MPU.io.out.macResult
  io.out.res := MPU.io.out.res
  io.out.valid := MPU.io.out.valid
  io.out.dest := Mux(MPU.io.out.macResult, macDestQueue.io.deq.bits, destinationQueue.io.destOut)
  io.out.reduce := MPU.io.out.macResult && (macDestQueue.io.deq.bits.rf === RegisterFileType.SREG)

  io.fwd.rs1 := in.rs1
  io.fwd.rs2 := in.rs2

  val validHead = {for(head <- destinationQueue.io.head) yield { //We don't need to stall if queue is non-empty but no entries in dQueue are invalid (this happens when the final value in dQueue is being output)
    head.valid
  }}.reduce((a,b) => a|b)

  io.ctrl.empty := destinationQueue.io.empty
  io.ctrl.op := op
  io.ctrl.queueHead := destinationQueue.io.head

  io.ctrl.queueHead(0).valid := destinationQueue.io.head(0).valid & !MPU.io.out.valid
    //Since the result from the execute stage is ready for forwarding on the nextclock cycle,
    // we do not need to assert the valid signal on the same clock cycle that the result comes out
}





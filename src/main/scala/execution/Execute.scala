package execution

import chisel3._
import utils.Config._
import Opcode._
import chisel3.util._
import utils.Fixed._

/**
 * I/O ports for the vector execution stage.
 */
class ExecuteIO extends Bundle {
  val id = Flipped(new IdExIO)
  val wb = new ExWbIO
  val ctrl = new ExControlIO
  val fwd = new ExFwdIO
}

/**
 * The vector execution stage of the pipeline. Implements [[ExecuteIO]].
 * The execute stage must not be supplied with new inputs of differing types while processing long instructions.
 * To ensure this, a signal must be asserted which tells it to keep the current op (io.ctrl.stall).
 * When stall is asserted, the previously asserted op will be kept, but the internal 'valid' signal will be forced false
 * Note that other inputs like a,b are allowed to flow into the execute stage, since a change in these values does not provoke a change in the system
 */
class Execute extends Module {
  val io = IO(new ExecuteIO)

  // --- MODULES ---
  val MPU = Module(new MatrixProcessingUnit(NUM_PROCELEM))
  val macDestQueue = Module(new util.Queue(new RegisterBundle(),8))
  val destinationQueue = Module(new DestinationQueue())

  // --- REGISTERS ---
  val in = RegNext(io.id)
  //Must delay stall signal by one cc, since valid + op from decode stage are also delayed by 1 due to
  //the SyncReadMem implementation of the register file
  val op = RegInit(Opcode.NOP)
  val valid = RegInit(false.B)
  val redVVcounter = RegInit(0.U((SUBVECTORS_PER_VREG+1).W))


  // --- LOGIC ---
  val validSignal =  !io.ctrl.stall && io.id.op =/= Opcode.NOP && io.id.valid
  valid := validSignal

  //Stall / NOP overrides to ensure we don't process anything
  val opSignal = Mux(io.ctrl.stall || io.id.op === NOP, op, io.id.op)
  op := opSignal

  //Select between forwarding values or original values
  //Notice: We are using values directly from ID stage since SyncReadMem delays memory reads by 1 clock cycle
  //All other signals are taken from pipeline register, but data is taken straight from ID input
  val a = Mux(io.fwd.rs1swap, io.fwd.rs1newData, io.id.a)
  val b = Mux(io.fwd.rs2swap, io.fwd.rs2newData, io.id.b)
  val immVec = Wire(Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W)))
  for(i <- 0 until NUM_PROCELEM) {
    immVec(i) := in.imm
  }
  // --- CONNECTIONS ---
  MPU.io.in.a := Mux(in.useImm, immVec, a)
  MPU.io.in.b := b
  MPU.io.in.valid := valid
  MPU.io.in.op := op
  MPU.io.in.macLimit := in.macLimit

  //We need a SEPARATE destination for MAC instructions, to allow other instructions to be processed at the same time
  destinationQueue.io.destIn := io.id.dest
  destinationQueue.io.enq := validSignal && !(opSignal === MAC || opSignal === RED)
  macDestQueue.io.enq.bits := io.id.dest

  //When inserting RED.XX instructions, every input is valid
  //When inserting RED.VV instructions, only every SUBVECTORS_PER_VREG'th input is valid (as all VREG_DEPTH elements go into one slot in the dest. X-register)
  //When inserting MAC.KV/MAC.VV/MAC.SV instructions, inputs are only valid if the dest. queue is empty
  redVVcounter := Mux(opSignal === RED && io.id.dest.rf === RegisterFileType.XREG && validSignal, //If op===RED and rf === XREG, instruction is RED.VV
    Mux(redVVcounter === (SUBVECTORS_PER_VREG-1).U, 0.U, redVVcounter + 1.U),
    redVVcounter)
  macDestQueue.io.enq.valid := validSignal && ((opSignal === RED && redVVcounter === 0.U) || (opSignal === MAC && macDestQueue.io.count === 0.U))


  //Output signals
  destinationQueue.io.deq := MPU.io.out.valid && !MPU.io.out.macResult
  macDestQueue.io.deq.ready := MPU.io.out.valid && MPU.io.out.macResult
  io.wb.res := MPU.io.out.res
  io.wb.valid := MPU.io.out.valid
  io.wb.dest := Mux(MPU.io.out.macResult, macDestQueue.io.deq.bits, destinationQueue.io.destOut)
  io.wb.reduce := MPU.io.out.macResult && (macDestQueue.io.deq.bits.rf === RegisterFileType.SREG || macDestQueue.io.deq.bits.rf === RegisterFileType.XREG)

  io.fwd.rs1 := in.rs1
  io.fwd.rs2 := in.rs2

  io.ctrl.empty := destinationQueue.io.empty
  io.ctrl.macEmpty := macDestQueue.io.count === 0.U
  io.ctrl.op := op
  io.ctrl.queueHead := destinationQueue.io.head

  //Since the result from the execute stage is ready for forwarding on the next clock cycle,
  // we do not need to assert the valid signal on the same clock cycle that the result comes out
  io.ctrl.queueHead(0).valid := destinationQueue.io.head(0).valid & !MPU.io.out.valid

}





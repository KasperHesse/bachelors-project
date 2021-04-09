package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util.log2Ceil
import utils.Config._
import utils.Fixed._
import vector.Opcode

/**
 * Interface between the instruction preview and instruction decode stages.
 * Instantiate as-is in the preview stage, use Flipped() in the decode stage
 */
class IpIdIO extends Bundle {
  val instr = Output(UInt(32.W))
}

/**
 * Interface between the instruction decode stage and the vector execution stage.
 * Instantiate as-is in the decode stage, and use Flipped() in the Ex stage
 */
class IdExIO extends Bundle {
  /** Vector of the first operands */
  val a = Output(Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W)))
  /** Vector of second operands */
  val b = Output(Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W)))
  /** Destination register and subvector of the result */
  val dest = Output(new Destination())
  /** Operation to execute. See [[vector.Opcode]] */
  val op = Output(Opcode())
  /** Number of multiply-accumulates to perform before releasing the result. Only used when MAC operations are performed.
   * Width 32 is currently a guess */
  val macLimit = Output(UInt(32.W))
  /** Signals to the execution unit that the incoming operation should be added to the destination queue.
   * Mostly useful for MAC operations, where multiple operations are performed for each destination input. */
  val newDest = Output(Bool())
}

/**
 * Interface between the instruction decode stage and memory.
 * Instantiate as-is in decode stage and use FLipped() in memory module
 */
class IdMemIO extends Bundle {
  val rdData = Input(Vec(KE_SIZE, SInt(FIXED_WIDTH.W)))
}

/**
 * Interface between the vector execution stage and the writeback stage.
 * Instantiate as-is in the execute stage and use Flipped() in the writeback stage
 */
class ExWbIO extends Bundle {
  /** The result produced in the execute stage */
  val res = Output(Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W)))
  /** Destination register and subvector of result */
  val dest = Output(new Destination())
  /** Indicates that the result is valid and should be stored in the register file */
  val valid = Output(Bool())
}

/**
 * Interface between the vector execution stage and the control module.
 * Use as-is in the execute stage, use Flipped() in control stage
 */
class ExControlIO extends Bundle {
  /** Number of elements in the destination queue / number of operations still processing */
  val count = Output(UInt(5.W))
  /** Whether the pipeline should be stalled. If true, deasserts valid for all operations going into MPU */
  val stall = Input(Bool())
  /** Opcode of the currently executing instruction */
  val op = Output(Opcode())
}

/**
 * Inteface between the instruction decode stage and the control module.
 * Use as-is in the decode stage, use Flipped() in the control module
 */
class IdControlIO extends Bundle {
  /** Asserted whenever the decode stage can load new instructions into its instruction buffer */
  val iload = Input(Bool()) //Should be when decode is idle and preview isntr is istart
  /** The current instruction load/execute stage */
  val state = Output(DecodeState())
  /** Value indicating whether thread 0 or thread 1 is the currently executing thread */
  val execThread = Output(UInt(1.W))
  /** Asserted when the decode unit should stall. This can either be because the Ex unit is not finished processing,
   * or because data has yet to be transferred from memory into the register file. */
  val stall = Input(Bool())
  /** Control signals originating from threads inside of decode stage */
  val threadCtrl = Vec(2, new ThreadControlIO)
}

/**
 * Interface between instruction fetch stage and control module
 * Use as-is in fetch stage, use Flipped() in control module
 */
class IfControlIO extends Bundle {
  /** The instruction currently being fetched from IM */
  val instr = Output(UInt(32.W))
  /** Stall signal */
  val stall = Input(Bool())
}

/**
 * Inteface between the instruction decode stage and the control module.
 * Use as-is in the decode stage, use Flipped() in the control module
 */
class IdControlOldIO extends Bundle {
  /** Asserted whenever the decode stage can load new instructions into its instruction buffer */
  val iload = Input(Bool())
  /** The current instruction load/execute stage */
  val state = Output(DecodeOldStage())
  /** Asserted during the final clock cycle of the current instruction */
  val finalCycle = Output(Bool())
  /** Asserted during the first clock cycle of the current instruction */
  val firstCycle = Output(Bool())
  /** Opcode of the currently decoding instruction */
  val op = Output(Opcode())
  /** R-type mod field of the currently decoding instruction */
  val rtypemod = Output(RtypeMod())
  /** Asserted when the decode unit should stall. This can either be because the Ex unit is not finished processing,
   * or because data has yet to be transferred from memory into the register file. */
  val stall = Input(Bool())
}

/**
 * Interface between threads and the control module.
 * Use as-is in the decode stage, use Flipped() in control module
 */
class ThreadControlIO extends Bundle {
  /** Current state for the thread */
  val state = Output(ThreadState())
  /** Current state as UInt, decoded for debug purposes */
  val stateUint = Output(UInt(8.W))
  /** Asserted during the final clock cycle of the current instruction */
  val finalCycle = Output(Bool())
  /** Asserted during the first clock cycle of the current instruction */
  val firstCycle = Output(Bool())
  /** Opcode of the currently executing Rtype instruction */
  val op = Output(Opcode())
  /** R-type mod field of the currently executing instruction */
  val rtypemod = Output(RtypeMod())
  /** Asserted when the unit should stall. This can either be because the Ex unit is not finished processing,
   * or because data has yet to be transferred from memory into the register file. */
  val stall = Input(Bool())
}

class DecodeOldIO extends Bundle {
  val ex = new IdExIO
  val mem = new IdMemIO
  val in = Flipped(new IpIdIO)
  val ctrl = new IdControlOldIO
}



/**
 * A bundle encoding the destination register and subvector for a result.
 */
class Destination extends Bundle {
  /** Destination register */
  val rd = UInt(log2Ceil(NUM_VECTOR_REGISTERS).W)
  /** Subvector of that destination register */
  val subvec = UInt(log2Ceil(VECTOR_REGISTER_DEPTH/NUM_PROCELEM).W)
  /** The register file that the result should be stored in.
   * When stored in the x-reg file, the subvector is ignored.
   * When stored in the scalar reg file, only the element at [0] is stored*/
  val rf = RegisterFileType()

  val rfUint = UInt(4.W)
}

object RegisterFileType extends ChiselEnum {
  val SREG, VREG, XREG = Value
}



package pipeline

import chisel3._
import chisel3.util.log2Ceil
import utils.Config._
import utils.Fixed._
import vector.ProcElemOpcode._

class Interfaces {

}

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
  /** Operation to execute. See [[vector.ProcElemOpcode]] */
  val op = Output(UInt(PE_OP_WIDTH.W))
  /** Number of multiply-accumulates to perform before releasing the result. Width 32 is currently a guess */
  val macLimit = Output(UInt(32.W))
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
}

/**
 * Inteface between the instruction decode stage and the control module.
 * Use as-is in the decode stage, use Flipped() in the control module
 */
class IdControlIO extends Bundle {

  /** Whether instructions should be loaded from the preview stage (true) or instructions should be executed (false) */
  val iload = Input(Bool())
  /** The current instruction load/execute stage */
  val state = Output(DecodeStage())
  /** Asserted during the final clock cycle of the current instruction mix, signalling that new instructions may be loaded */
  val finalCycle = Output(Bool())
}

/**
 * A bundle encoding the destination register and subvector for a result.
 */
class Destination extends Bundle {
  /** Destination register */
  val rd = UInt(log2Ceil(NUM_VECTOR_REGISTERS).W)
  /** Subvector of that destination register */
  val subvec = UInt(log2Ceil(VECTOR_REGISTER_DEPTH/NUM_PROCELEM).W)
}



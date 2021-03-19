package vector
import chisel3._
import chisel3.experimental.ChiselEnum

//TODO: Rework the opcodes to make use of the ChiselEnum class (chisel3.experimental)
/**
 * Opcodes supported by a processing element.
 */
object ProcElemOpcode {
  val ADD_P = 0
  val SUB_P = 1
  val MUL_P = 2
  val DIV_P = 3
  val MAC_P = 4

  /** Required width of a UInt field that takes an Opcode */
  val PE_OP_WIDTH = 5 //Width of a UInt field that should take an opcode

  //Defining positions and using bitshift to create more manageable one-hot encoding
  val NOP = 0.U
  val ADD = (1 << ADD_P).U
  val SUB = (1 << SUB_P).U
  val MUL = (1 << MUL_P).U
  val DIV = (1 << DIV_P).U
  val MAC = (1 << MAC_P).U
}
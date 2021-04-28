package vector
import chisel3._
import chisel3.experimental.ChiselEnum

//TODO: Rework the opcodes to make use of the ChiselEnum class (chisel3.experimental)
/**
 * Opcodes supported by a processing element.
 */
object Opcode extends ChiselEnum {
  val NOP = Value("b000000".U)
  val ADD = Value("b000100".U)
  val SUB = Value("b000101".U)
  val MAX = Value("b000110".U)
  val MIN = Value("b000111".U)
  val MUL = Value("b001000".U)
  val MAC = Value("b011000".U)
  val DIV = Value("b100000".U)

}
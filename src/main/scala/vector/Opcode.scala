package vector
import chisel3._
import chisel3.experimental.ChiselEnum

/**
 * Opcodes supported by a processing element.
 */
object Opcode extends ChiselEnum {
  val NOP = Value(0.U)
  val ADD = Value(1.U)
  val SUB = Value(0x2.U)
  val MUL = Value(0x4.U)
  val DIV = Value(0x8.U)
  val MAC = Value(0x10.U)
  val WIDTH = Value(0xfff.U)
}
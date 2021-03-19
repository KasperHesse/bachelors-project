package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum


class Instruction extends Bundle {
  val value = UInt(32.W)
}

/**
 * A bundle defining the fields that constitute an R-type instruction
 */
class RtypeInstruction extends Bundle {
  val rd = UInt(5.W)
  val rs2 = UInt(5.W)
  val rs1 = UInt(5.W)
  val mod = RtypeMod()
  val fmt = InstructionFMT()
  val op = UInt(12.W)

  def fromUInt(v: UInt): RtypeInstruction = {
    val b = new RtypeInstruction
    b.op := v(11,0)
    b.fmt := InstructionFMT(v(13,12))
    b.mod := RtypeMod(v(16,4))
    b.rs1 := v(21,17)
    b.rs2 := v(26,22)
    b.rd := v(31,27)
    b
  }
}

/**
 * A bundle defining the fields that constitute an S-type instruction
 */
class StypeInstruction extends Bundle {
  val nu = UInt(10.W)
  val rsrd = UInt(5.W)
  val mod = StypeMod()
  val fmt = InstructionFMT()
  val offset = StypeOffset()
}

/**
 * A bundle defining the fields that constitute an O-type instruction
 */
class OtypeInstruction extends Bundle {
  val nu2 = UInt(18.W)
  val fmt = InstructionFMT()
  val nu1 = UInt(10.W)
  val iev = OtypeIEV()
  val se = OtypeSE()
}

/**
 * Defines the various instruction formats available
 */
object InstructionFMT extends ChiselEnum {
  val RTYPE = Value(0.U)
  val STYPE = Value(1.U)
  val OTYPE = Value(2.U)
}

/**
 * Defines the various modifiers for R-type instructions. These modifiers are mainly used to select where the first
 * and second operands come from.
 */
object RtypeMod extends ChiselEnum {
  val VV = Value("b000".U)
  val XV = Value("b010".U)
  val XX = Value("b011".U)
  val SV = Value("b100".U)
  val SS = Value("b101".U)
  val MVP = Value("b111".U)
}

/**
 * Defines the various modifiers for S-type instructions. These define whether a load/store should be performed,
 * and what kind of load/store operation to do.
 */
object StypeMod extends ChiselEnum {
  val LDVEC = Value("b000".U)
  val LDDOF = Value("b001".U)
  val LDSCA = Value(2.U)
  val LDELEM = Value(3.U)
  val STVEC = Value(4.U)
  val STDOF = Value(5.U)
  val STSCA = Value(6.U)
  val STELEM = Value(7.U)
}

/**
 * Defines values that make decoding base memory addresses for the various memory regions easier. The actual decoding
 * is performed in hardware
 */
object StypeOffset extends ChiselEnum {
  val X, XPHYS, XNEW, DC, DV, F, U, R, Z, P, Q, INVD, TMP = Value
  val WIDTH = Value(0xfff.U)
}

/**
 * Defines the instrution/element/vector flags for O-type instructions
 */
object OtypeIEV extends ChiselEnum {
  val INSTR = Value(1.U)
  val ELEM = Value(2.U)
  val VEC = Value(3.U)
}

/**
 * Defines the begin/end flags used for O-type instructions
 */
object OtypeSE extends ChiselEnum {
  val END = Value(0.U)
  val START = Value(1.U)
}

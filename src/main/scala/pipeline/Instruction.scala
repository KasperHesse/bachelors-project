package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import vector.Opcode


trait Instruction {
  def toUInt(): UInt
//  def name: String
}

/**
 * A bundle defining the fields that constitute an R-type instruction
 */
class RtypeInstruction extends Bundle with Instruction {
  val rd = UInt(5.W)
  val rs2 = UInt(5.W)
  val rs1 = UInt(5.W)
  val mod = RtypeMod()
  val fmt = InstructionFMT()
  val op = Opcode()

  override def toUInt(): UInt = {
    RtypeInstruction.apply(this)
  }
}

object RtypeInstruction {
  def apply(rd: Int, rs1: Int, rs2: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> rs1.U, _.rs2 -> rs2.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE)
  }

  def apply(v: RtypeInstruction): UInt = {
    var r = 0
    r |= (v.op.litValue.toInt)
    r |= (v.fmt.litValue().toInt << 12)
    r |= (v.mod.litValue.toInt << 14)
    r |= (v.rs1.litValue().toInt << 17)
    r |= (v.rs2.litValue().toInt << 22)
    r |= (v.rd.litValue().toInt << 27)
    r.U(32.W)
  }

}

/**
 * A bundle defining the fields that constitute an S-type instruction
 */
class StypeInstruction extends Bundle with Instruction {
  val nu = UInt(10.W)
  val rsrd = UInt(5.W)
  val mod = StypeMod()
  val fmt = InstructionFMT()
  val offset = StypeOffset()

  override def toUInt(): UInt = {
    StypeInstruction.apply(this)
  }

}

object StypeInstruction {
  def apply(v: StypeInstruction): UInt = {
    var s = 0
    s |= (v.offset.litValue().toInt)
    s |= (v.fmt.litValue().toInt << 12)
    s |= (v.mod.litValue().toInt << 14)
    s |= (v.rsrd.litValue().toInt << 17)
    s.U(32.W)
  }

  def apply(rsrd: Int, mod: StypeMod.Type, offset: StypeOffset.Type): StypeInstruction = {
    (new StypeInstruction).Lit(_.rsrd -> rsrd.U, _.mod -> mod, _.offset -> offset, _.fmt -> InstructionFMT.STYPE)
  }
}

/**
 * A bundle defining the fields that constitute an O-type instruction
 */
class OtypeInstruction extends Bundle with Instruction {
  val nu2 = UInt(18.W)
  val fmt = InstructionFMT()
  val nu1 = UInt(10.W)
  val iev = OtypeIEV()
  val se = OtypeSE()

  override def toUInt(): UInt = {
    OtypeInstruction.apply(this)
  }
}

object OtypeInstruction extends Bundle {
  def apply(se: OtypeSE.Type, iev: OtypeIEV.Type): OtypeInstruction = {
    (new OtypeInstruction).Lit(_.se -> se, _.iev -> iev, _.fmt -> InstructionFMT.OTYPE)
  }

  def apply(v: OtypeInstruction): UInt = {
    var o = 0
    o |= (v.se.litValue.toInt)
    o |= (v.iev.litValue().toInt << 1)
    o |= (v.fmt.litValue().toInt << 12)
    o.U(32.W)
  }
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

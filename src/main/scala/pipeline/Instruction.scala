package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import vector.Opcode
import vector.Opcode._

/**
 * Defines an instruction. Useful for packaging instructions of different formats inside Scala collections
 */
trait Instruction {
  def toUInt(): UInt
}

/**
 * A bundle defining the fields that constitute an R-type instruction
 */
class RtypeInstruction extends Bundle with Instruction {
  /** Not used */
  val nu = UInt(2.W)
  /** Destination register */
  val rd = UInt(4.W)
  /** Source register 1 */
  val rs2 = UInt(4.W)
  /** Source register 2 */
  val rs1 = UInt(4.W)
  /** R-type modifier */
  val mod = RtypeMod()
  /** Instruction format */
  val fmt = InstructionFMT()
  /** Opcode */
  val op = Opcode()

  override def toUInt(): UInt = {
    RtypeInstruction.apply(this)
  }
}

object RtypeInstruction {
  def apply(rd: Int, rs1: Int, rs2: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> rs1.U, _.rs2 -> rs2.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE, _.nu -> 0.U(2.W))
  }

  def apply(v: RtypeInstruction): UInt = {
    var r = 0
    r |= (v.op.litValue.toInt)
    r |= (v.fmt.litValue().toInt << 12)
    r |= (v.mod.litValue.toInt << 14)
    r |= (v.rs1.litValue().toInt << 18)
    r |= (v.rs2.litValue().toInt << 22)
    r |= (v.rd.litValue().toInt << 26)
    r.U(32.W)
  }

  /**
   * Converts a UInt to an Rtype instruction
   * @param v The UInt holding the instruction
   * @return The Rtype instruction with the decoded fields
   */
  def apply(v: UInt): RtypeInstruction = {
    //Beware, ugly code below. May be possible to optimize in some way, but it's what we have to work with
    import RtypeMod._
    val opval = v(11,0)
    val modval = v(17,14)

    val op: Opcode.Type = if(opval.litValue == ADD.litValue()) {
      ADD
    } else if (opval.litValue == SUB.litValue()) {
      SUB
    } else if (opval.litValue == MUL.litValue()) {
      MUL
    } else if (opval.litValue == DIV.litValue()) {
      DIV
    } else if (opval.litValue == MAC.litValue()) {
      MAC
    } else {
      throw new IllegalArgumentException("Unable to decode op")
    }

    val fmt = InstructionFMT.RTYPE
    val mod = if(modval.litValue == VV.litValue()) {
      VV
    } else if (modval.litValue == XV.litValue()) {
      XV
    } else if (modval.litValue() == SV.litValue()) {
      SV
    } else if (modval.litValue() == SS.litValue()) {
      SS
    } else if (modval.litValue() == KV.litValue()) {
      KV
    } else if (modval.litValue() == XX.litValue()) {
      XX
    } else if(modval.litValue() == SX.litValue()) {
      SX
    } else {
      throw new IllegalArgumentException("Unable to decode mod")
    }
    (new RtypeInstruction).Lit(_.op -> op, _.fmt -> fmt, _.mod -> mod, _.rs1 -> v(21,18), _.rs2 -> v(25,22), _.rd -> v(29,26))
  }

}

/**
 * A bundle defining the fields that constitute an S-type instruction
 */
class StypeInstruction extends Bundle with Instruction {
  /** Not used */
  val nu = UInt(10.W) //31:22
  /** Register source (when storing) or destination register (when loading) */
  val rsrd = UInt(4.W) //21:18
  /** S-type modifier */
  val mod = StypeMod() //17:14
  /** Instruction format */
  val fmt = InstructionFMT() //13:12
  /** S-type offset */
  val offset = StypeOffset() //11:0

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
  /** Not used */
  val nu2 = UInt(18.W)        //31:14
  /** Instruction format */
  val fmt = InstructionFMT()  //13:12
  /** Not used */
  val nu1 = UInt(6.W)         //11:6
  /** Increment type */
  val it = Bool()             //5
  /** Instruction length */
  val len = OtypeLen()        //4:3
  /** Packet/execute flag */
  val iev = OtypeIEV()        //2:1
  /** Start/end flag */
  val se = OtypeSE()          //0

  override def toUInt(): UInt = {
    OtypeInstruction.apply(this)
  }

  def toInt(): Int = {
    OtypeInstruction.apply(this).litValue.toInt
  }
}

object OtypeInstruction extends Bundle {
  /** Generates an Otype-instruction with length [[OtypeLen.SINGLE]] */
  def apply(se: OtypeSE.Type, iev: OtypeIEV.Type): OtypeInstruction = {
    apply(se, iev, OtypeLen.SINGLE)
  }
  /** Generates an Otype instruction with a specified length */
  def apply(se: OtypeSE.Type, iev: OtypeIEV.Type, len: OtypeLen.Type): OtypeInstruction = {
    (new OtypeInstruction).Lit(_.se -> se, _.iev -> iev, _.len -> len, _.fmt -> InstructionFMT.OTYPE, _.it -> false.B)
  }

  /**
   * Returns a UInt representing the Otype-instruction given by v
   * @param v An O-type instruction
   * @return A UInt representing these fields
   */
  def apply(v: OtypeInstruction): UInt = {
    var o = 0
    o |= (v.se.litValue.toInt)
    o |= (v.iev.litValue().toInt << 1)
    o |= (v.len.litValue().toInt << 3)
    o |= (v.it.litValue.toInt << 5)
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
  val VV = Value("b0000".U)
  val XV = Value("b0100".U)
  val XX = Value("b0101".U)
  val SV = Value("b1000".U)
  val SX = Value("b1001".U)
  val SS = Value("b1010".U)
  val KV = Value("b1100".U) //11 = KE, 00 = V
}

/**
 * Defines the various modifiers for S-type instructions. These define whether a load/store should be performed,
 * and what kind of load/store operation to do.
 */
object StypeMod extends ChiselEnum {
  val LDVEC =  Value("b0000".U)
  val LDDOF =  Value("b0001".U)
  val LDSCA =  Value("b0010".U)
  val LDELEM = Value("b0011".U)
  val STVEC =  Value("b1100".U)
  val STDOF =  Value("b1101".U)
  val STSCA =  Value("b1110".U)
  val STELEM = Value("b1111".U)
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
  val EXEC = Value(2.U)
  val VEC = Value(3.U)
}

/**
 * Defines the begin/end flags used for O-type instructions
 */
object OtypeSE extends ChiselEnum {
  val END = Value(0.U)
  val START = Value(1.U)
}

/**
 * Defines the length of vectors used in an instruction
 */
object OtypeLen extends ChiselEnum {
  val NDOF = Value(0.U)
  val NELEM = Value(1.U)
  val SINGLE = Value(2.U)
}

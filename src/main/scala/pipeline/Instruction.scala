package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import vector.Opcode
import vector.Opcode._
import utils.Config.{NUM_SREG}

/**
 * Defines an instruction. Useful for packaging instructions of different formats inside Scala collections
 */
trait Instruction {
  /** Converts the instruction into its' UInt representation */
  def toUInt(): UInt
}

/**
 * A bundle defining the fields that constitute an R-type instruction
 */
class RtypeInstruction extends Bundle with Instruction {
  /** Fractional part of immediate. Only used when immflag = true */
  val immfrac = UInt(7.W) //31:25
  /** Source register 2 */
  val rs2 = UInt(4.W) //24:21
  /** Source register 1 */
  val rs1 = UInt(4.W) //20:17
  /** Destination register */
  val rd = UInt(4.W) //16:13
  /** Immediate flag */
  val immflag = Bool() //12
  /** R-type modifier */
  val mod = RtypeMod() //11:8
  /** Instruction format */
  val fmt = InstructionFMT() //7:6
  /** Opcode */
  val op = Opcode() //5:0

  override def toUInt(): UInt = {
    RtypeInstruction.apply(this)
  }
}

object RtypeInstruction {
  /** Creates an R-type instruction which takes two register operands */
  def apply(rd: Int, rs1: Int, rs2: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> rs1.U, _.rs2 -> rs2.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE, _.immflag -> false.B, _.immfrac -> 0.U(7.W))
  }

  /** Creates an R-type instruction which takes a single register operand and an immediate */
  def apply(rd: Int, rs1: Int, immh: Int, frac: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    require(immh < 16, "Integer part of immediate must be less than 16")
    require(frac < 128, "Fractional part of immediate must be less than 128")
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> rs1.U, _.rs2 -> immh.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE, _.immflag -> true.B, _.immfrac -> frac.U)
  }

  /** Converts an R-type instruction to its UInt representation */
  def apply(v: RtypeInstruction): UInt = {
    var r: Long = 0
    r |= (v.op.litValue.toInt)
    r |= (v.fmt.litValue().toInt << 6)
    r |= (v.mod.litValue.toInt << 8)
    r |= (v.immflag.litValue.toInt << 12)
    r |= (v.rd.litValue().toInt << 13)
    r |= (v.rs1.litValue().toInt << 17)
    r |= (v.rs2.litValue().toInt << 21)
    r |= (v.immfrac.litValue.toLong << 25)
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
    val opval = v(5,0)
    val modval = v(11,8)
    val immfrac = v(31,25)
    val rd = v(16,13)
    val rs1 = v(20,17)
    val rs2 = v(24,21)

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
    } else if (opval.litValue == MAX.litValue()) {
      MAX
    }else if (opval.litValue == MIN.litValue()) {
      MIN
    } else {
      print("ERR: Unable to decode op")
//      throw new IllegalArgumentException("Unable to decode op")
      ADD
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
      print("ERR: Unable to decode mod")
//      throw new IllegalArgumentException("Unable to decode op")
      VV
    }
    (new RtypeInstruction).Lit(_.op -> op, _.fmt -> fmt, _.mod -> mod, _.rs1 -> rs1, _.rs2 -> rs2, _.rd -> rd, _.immflag -> v(12), _.immfrac -> immfrac)
  }

}

/**
 * A bundle defining the fields that constitute an S-type instruction
 */
class StypeInstruction extends Bundle with Instruction {
  /** Not used */
  val nu1 = UInt(15.W) //31:17
  /** Register source (when storing) or destination register (when loading) */
  val rsrd = UInt(4.W) //16:13
  /** Not used */
  val nu2 = UInt(1.W) //12
  /** S-type modifier */
  val mod = StypeMod() //11:8
  /** Instruction format */
  val fmt = InstructionFMT() //7:6
  /** S-type offset */
  val offset = StypeOffset() //5:0

  override def toUInt(): UInt = {
    StypeInstruction.apply(this)
  }
}

object StypeInstruction {
  /** Converts an S-type instruction to it's UInt representation */
  def apply(v: StypeInstruction): UInt = {
    var s = 0
    s |= (v.offset.litValue().toInt)
    s |= (v.fmt.litValue().toInt << 6)
    s |= (v.mod.litValue().toInt << 8)
    s |= (v.rsrd.litValue().toInt << 13)
    s.U(32.W)
  }

  /** Constructs an S-type instruction from the given parameters */
  def apply(rsrd: Int, mod: StypeMod.Type, offset: StypeOffset.Type): StypeInstruction = {
    (new StypeInstruction).Lit(_.rsrd -> rsrd.U, _.mod -> mod, _.offset -> offset, _.fmt -> InstructionFMT.STYPE)
  }
}

/**
 * A bundle defining the fields that constitute an O-type instruction
 */
class OtypeInstruction extends Bundle with Instruction {
  /** Not used */
  val nu2 = UInt(24.W)        //31:8
  /** Instruction format */
  val fmt = InstructionFMT()  //7:6
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
    o |= (v.fmt.litValue().toInt << 6)
    o.U(32.W)
  }

  def apply(v: UInt): OtypeInstruction = {
    val seval = v(0).litToBoolean
    val ievval = v(2,1).litValue.toInt
    val lenval = v(4,3).litValue.toInt
    val it = v(5)

    val se = seval match {
      case false => OtypeSE.END
      case true => OtypeSE.START
    }

    val iev = ievval match {
      case 1 => OtypeIEV.INSTR
      case 2 => OtypeIEV.EXEC
      case _ => OtypeIEV.INSTR //This shouldn't happen
    }

    val len = lenval match {
      case 0 => OtypeLen.NDOF
      case 1 => OtypeLen.NELEM
      case 2 => OtypeLen.SINGLE
      case _ => OtypeLen.SINGLE //This shouldn't happen
    }

    (new OtypeInstruction).Lit(_.fmt -> InstructionFMT.OTYPE, _.se -> se, _.iev -> iev, _.len -> len, _.it -> it)
  }
}

class BtypeInstruction extends Bundle with Instruction {
  /** Upper bits of branch target */
  val targeth = UInt(7.W) //31:25
  /** Register source for comparison */
  val rs2 = UInt(4.W) //24:21
  /** Register source 1 for comparison */
  val rs1 = UInt(4.W) //20:17
  /** Lower bits of branch target */
  val targetl = UInt(9.W) //16:8
  /** Instruction format */
  val fmt = InstructionFMT() //7:6
  /** Branch comparsion operation */
  val comp = BranchComp() //5:0

  override def toUInt(): UInt = {
    BtypeInstruction.apply(this)
  }
}

object BtypeInstruction {
  /** Generates a B-type instruction from the given parameters */
  def apply(comp: BranchComp.Type, rs1: Int, rs2: Int, offset: Int): BtypeInstruction = {
    //Convert branch target to correct representation
    require(-math.pow(2,17)<= offset && offset < math.pow(2,17)-4, "Offset must be in range (-1)*2^17 : 2^17-4")
    require(offset % 4 == 0, "Branch target offset must be a multiple of 4")
    require(rs1 < NUM_SREG)
    require(rs2 < NUM_SREG)
    //Extract bits of target
    val targetl = (offset & 0x7fc) >> 2
    val targeth = (offset & 0x3f800) >> 11
    (new BtypeInstruction).Lit(_.fmt -> InstructionFMT.BTYPE, _.comp -> comp, _.rs1 -> rs1.U, _.rs2 -> rs2.U, _.targetl -> targetl.U, _.targeth -> targeth.U)
  }
  /**
   * Converts a B-type instruction to it's UInt representation
   * @param v The b-type instruction to convert
   * @return The UInt value of that instruction
   */
  def apply(v: BtypeInstruction): UInt = {
    var b: Long = 0
    b |= (v.comp.litValue().toInt)
    b |= (v.fmt.litValue.toInt << 6)
    b |= (v.targetl.litValue().toInt << 8)
    b |= (v.rs1.litValue.toInt << 17)
    b |= (v.rs2.litValue.toInt << 21)
    b |= (v.targeth.litValue.toLong << 25)
    b.U(32.W)
  }

  /**
   * Converts a UInt to the corresponding B-type instruction
   * @param v The UInt
   * @return A B-type instruction with those values
   */
  def apply(v: UInt): BtypeInstruction = {
    val compvalue = v(1,0).litValue.toInt
    val targetl = v(16,8)
    val targeth = v(31,25)
    val rs1 = v(20,17)
    val rs2 = v(24,21)

    val comp = compvalue match {
      case 0 => BranchComp.EQUAL
      case 1 => BranchComp.NEQ
      case 2 => BranchComp.LT
      case 3 => BranchComp.GEQ
      case _ => throw new IllegalArgumentException("Unable to decode comp value")
    }
    (new BtypeInstruction).Lit(_.fmt -> InstructionFMT.BTYPE, _.comp -> comp, _.rs1 -> rs1, _.rs2 -> rs2, _.targetl -> targetl, _.targeth -> targeth)
  }
}

/**
 * Defines the various instruction formats available
 */
object InstructionFMT extends ChiselEnum {
  val RTYPE = Value(0.U)
  val STYPE = Value(1.U)
  val OTYPE = Value(2.U)
  val BTYPE = Value(3.U)

  def apply(v: Int): this.Type = {
    v match {
      case 0 => InstructionFMT.RTYPE
      case 1 => InstructionFMT.STYPE
      case 2 => InstructionFMT.OTYPE
      case 3 => InstructionFMT.BTYPE
      case _ => throw new IllegalArgumentException("Unable to decode instruction format")
    }
  }
}

/**
 * Defines the various modifiers for R-type instructions. These modifiers are mainly used to select where the first
 * and second operands come from.
 */
object RtypeMod extends ChiselEnum {
  //00 = V
  //01 = X
  //10 = S
  //11 = K
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
  val WIDTH = Value("b111111".U)
}

/**
 * Defines the instrution/element/vector flags for O-type instructions
 */
object OtypeIEV extends ChiselEnum {
  val INSTR = Value(1.U)
  val EXEC = Value(2.U)
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

object BranchComp extends ChiselEnum {
  val EQUAL = Value("b000000".U)
  val NEQ = Value("b000001".U)
  val LT = Value("b000010".U)
  val GEQ = Value("b000011".U)
  val WIDTH = Value("b111111".U)
}

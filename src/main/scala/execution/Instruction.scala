package execution

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import utils.Config.NUM_SREG

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
  /** Source register 1 */
  val rs1 = UInt(4.W) //31:28
  /** Fractional part of immediate. Only used when immflag = true */
  val immfrac = UInt(7.W) //21:27
  /** Source register 2 */
  val rs2 = UInt(4.W) //20:17
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
  import Opcode._
  val OP_OFFSET = 0
  val FMT_OFFSET = 6
  val MOD_OFFSET = 8
  val IMMFLAG_OFFSET = 12
  val RD_OFFSET = 13
  val RS2_OFFSET = 17
  val FRAC_OFFSET = 21
  val RS1_OFFSET = 28

  /** Creates an R-type instruction which takes two register operands */
  def apply(rd: Int, rs1: Int, rs2: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> rs1.U, _.rs2 -> rs2.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE, _.immflag -> false.B, _.immfrac -> 0.U(7.W))
  }

  /** Creates an R-type instruction which takes a single register operand and an immediate */
  def apply(rd: Int, rs2: Int, immh: Int, frac: Int, op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    require(immh < 16, "Integer part of immediate must be less than 16")
    require(frac < 128, "Fractional part of immediate must be less than 128")
    (new RtypeInstruction).Lit(_.rd -> rd.U, _.rs1 -> immh.U, _.rs2 -> rs2.U, _.op -> op, _.mod -> mod, _.fmt -> InstructionFMT.RTYPE, _.immflag -> true.B, _.immfrac -> frac.U)
  }

  /** Converts an R-type instruction to its UInt representation */
  def apply(v: RtypeInstruction): UInt = {
    var r: Long = 0
    r |= (v.op.litValue.toInt)
    r |= (v.fmt.litValue().toInt << FMT_OFFSET)
    r |= (v.mod.litValue.toInt << MOD_OFFSET)
    r |= (v.immflag.litValue.toInt << IMMFLAG_OFFSET)
    r |= (v.rd.litValue().toInt << RD_OFFSET)
    r |= (v.rs1.litValue().toLong << RS1_OFFSET)
    r |= (v.rs2.litValue().toInt << RS2_OFFSET)
    r |= (v.immfrac.litValue.toLong << FRAC_OFFSET)
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
    val rd = v(16,13)
    val rs2 = v(20,17)
    val immfrac = v(27,21)
    val rs1 = v(31,28)

    val fmt = v(7,6).litValue().toInt
    if(fmt != InstructionFMT.RTYPE.litValue.toInt) {
      throw new IllegalArgumentException(s"Instruction format ($fmt) did not match R-type format")
    }

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
    } else if (opval.litValue == MIN.litValue()) {
      MIN
    } else if (opval.litValue == ABS.litValue()) {
      ABS
    } else if(opval.litValue == RED.litValue()) {
      RED
    } else {
      throw new IllegalArgumentException("Unable to decode op")
    }

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
//      print("ERR: Unable to decode mod")
      throw new IllegalArgumentException("Unable to decode op")
//      VV
    }
    (new RtypeInstruction).Lit(_.op -> op, _.fmt -> InstructionFMT.RTYPE, _.mod -> mod, _.rs1 -> rs1, _.rs2 -> rs2, _.rd -> rd, _.immflag -> v(12), _.immfrac -> immfrac)
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
  /** S-type load/store flag */
  val ls = StypeLoadStore() //12
  /** S-type modifier */
  val mod = StypeMod() //11:8
  /** Instruction format */
  val fmt = InstructionFMT() //7:6
  /** S-type offset */
  val baseAddr = StypeBaseAddress() //5:0

  override def toUInt(): UInt = {
    StypeInstruction.apply(this)
  }
}

object StypeInstruction {
  import StypeBaseAddress._
  import StypeMod._
  import StypeLoadStore._
  val BASEADDR_OFFSET = 0
  val FMT_OFFSET = 6
  val MOD_OFFSET = 8
  val LS_OFFSET = 12
  val RSRD_OFFSET =  13
  /** Converts an S-type instruction to it's UInt representation */
  def apply(v: StypeInstruction): UInt = {
    var s = 0
    s |= (v.baseAddr.litValue().toInt)
    s |= (v.fmt.litValue().toInt << FMT_OFFSET)
    s |= (v.mod.litValue().toInt << MOD_OFFSET)
    s |= (v.ls.litValue.toInt << LS_OFFSET)
    s |= (v.rsrd.litValue().toInt << RSRD_OFFSET)
    s.U(32.W)
  }

  /**
   * Constructs an S-type instruction from the given parameters
   * @param rsrd The destination/source register of the load/store operation
   * @param mod The s-type modifier for the operation
   * @param baseAddr The base address for the load/store
   * @param ls Whether the operation is load or store. Defaults to LOAD
   * @return An Stype instruction bundle with all fields set
   */
  def apply(rsrd: Int, mod: StypeMod.Type, baseAddr: StypeBaseAddress.Type, ls: StypeLoadStore.Type = LOAD): StypeInstruction = {
    require(!(ls == LOAD && mod == FDOF), "You cannot perform ld.fdof operations")
    require(!(ls == STORE && Seq(FCN, EDN1, EDN2).contains(mod)), "Cannot perform st.fcn, st.edn1 and st.edn2 operations")
    (new StypeInstruction).Lit(_.rsrd -> rsrd.U, _.mod -> mod, _.baseAddr -> baseAddr, _.fmt -> InstructionFMT.STYPE, _.ls -> ls, _.nu1 -> 0.U)
  }

  /** Converts a UInt to the corresponding Stype instruction */
  def apply(v: UInt): StypeInstruction = {
    val baseAddrVal = v(6,0).litValue.toInt
    val modVal = v(11,8).litValue.toInt
    val lsVal = v(12).litToBoolean
    val rsrd = v(16,13)
    val fmt = v(7,6).litValue.toInt

    if(fmt != InstructionFMT.STYPE.litValue.toInt) {
      throw new IllegalArgumentException(s"Instruction format ($fmt) did not match S-type format")
    }

    val BaseAddr = baseAddrVal match {
      case 0 => X
      case 1 => XPHYS
      case 2 => XNEW
      case 3 => DC
      case 4 => DV
      case 5 => F
      case 6 => U
      case 7 => R
      case 8 => Z
      case 9 => P
      case 10 => Q
      case 11 => INVD
      case 12 => TMP
      case _ => throw new IllegalArgumentException(s"Unable to decode S-type base address (got $baseAddrVal)")
    }

    val mod = modVal match {
      case 0x0 => VEC
      case 0x1 => DOF
      case 0x2 => ELEM
      case 0x4 => EDN1
      case 0x5 => EDN2
      case 0x6 => FCN
      case 0x7 => SEL
      case _ => throw new IllegalArgumentException(s"Unable to decode S-type modifier (got $modVal)")
    }

    val ls = lsVal match {
      case false => LOAD
      case true => STORE
    }

    (new StypeInstruction).Lit(_.mod -> mod, _.baseAddr -> BaseAddr, _.ls -> ls, _.rsrd -> rsrd, _.fmt -> InstructionFMT.STYPE)
  }
}

/**
 * A bundle defining the fields that constitute an O-type instruction
 */
class OtypeInstruction extends Bundle with Instruction {
  /** Not used */
  val nu = UInt(19.W) //31:13
  /** Start/end flag */
  val se = OtypeSE() //12
  /** Modifier (packet/execute flag) */
  val mod = OtypeMod() //11:8
  /** Instruction format */
  val fmt = InstructionFMT() //7:6
  /** Instruction length */
  val len = OtypeLen() //5:0

  override def toUInt(): UInt = {
    OtypeInstruction.apply(this)
  }

  def toInt(): Int = {
    OtypeInstruction.apply(this).litValue.toInt
  }
}

object OtypeInstruction extends Bundle {
  val SE_OFFSET = 12
  val MOD_OFFSET = 8
  val LEN_OFFSET = 0
  val FMT_OFFSET = 6
  /** Generates an Otype-instruction with length [[OtypeLen.SINGLE]] */
  def apply(se: OtypeSE.Type, mod: OtypeMod.Type): OtypeInstruction = {
    apply(se, mod, OtypeLen.SINGLE)
  }
  /** Generates an Otype instruction with a specified length */
  def apply(se: OtypeSE.Type, mod: OtypeMod.Type, len: OtypeLen.Type): OtypeInstruction = {
    (new OtypeInstruction).Lit(_.se -> se, _.mod -> mod, _.len -> len, _.fmt -> InstructionFMT.OTYPE, _.nu -> 0.U)
  }

  /**
   * Returns a UInt representing the Otype-instruction given by v
   * @param v An O-type instruction
   * @return A UInt representing these fields
   */
  def apply(v: OtypeInstruction): UInt = {
    var o = 0
    o |= (v.len.litValue().toInt << LEN_OFFSET)
    o |= (v.fmt.litValue().toInt << FMT_OFFSET)
    o |= (v.mod.litValue().toInt << MOD_OFFSET)
    o |= (v.se.litValue.toInt << SE_OFFSET)

    o.U(32.W)
  }

  def apply(v: UInt): OtypeInstruction = {
    val seval = v(12).litToBoolean
    val modval = v(11,8).litValue.toInt
    val lenval = v(5,0).litValue.toInt

    val fmt = v(7,6).litValue().toInt
    if(fmt != InstructionFMT.OTYPE.litValue.toInt) {
      throw new IllegalArgumentException(s"Instruction format ($fmt) did not match O-type format (${InstructionFMT.OTYPE.litValue.toInt})")
    }

    val se = if (seval) {
      OtypeSE.START
    } else {
      OtypeSE.END
    }

    val mod = modval match {
      case 1 => OtypeMod.PACKET
      case 2 => OtypeMod.EXEC
      case 4 => OtypeMod.TIME
      case _ => throw new IllegalArgumentException(s"Unable to decode O-type modifier ($modval)") //This shouldn't happen
    }

    val len = lenval match {
      case 0 => OtypeLen.NDOF
      case 2 => OtypeLen.SINGLE
      case 4 => OtypeLen.NELEMVEC
      case 5 => OtypeLen.NELEMDOF
      case 6 => OtypeLen.NELEMSTEP
      case _ => throw new IllegalArgumentException(s"Unable to decode O-type length ($lenval)") //This shouldn't happen
    }

    (new OtypeInstruction).Lit(_.fmt -> InstructionFMT.OTYPE, _.se -> se, _.mod -> mod, _.len -> len, _.nu -> 0.U)
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
  val COMP_OFFSET = 0
  val FMT_OFFSET = 6
  val TARGETL_OFFSET = 8
  val RS1_OFFSET = 17
  val RS2_OFFSET = 21
  val TARGETH_OFFSET = 25
  val TARGETL_WIDTH = 9
  val TARGETH_WIDTH = 7

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

    val fmt = v(7,6).litValue().toInt
    if(fmt != InstructionFMT.BTYPE.litValue.toInt) {
      throw new IllegalArgumentException(s"Instruction format ($fmt) did not match B-type format")
    }

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
 * Defines the various modifiers for S-type instructions.
 * These define what kind of load/store operation should be performed
 */
object StypeMod extends ChiselEnum {
  val ELEM =  Value("b0010".U)
  val EDN1 =  Value("b0100".U)
  val EDN2 =  Value("b0101".U)
  val FCN =   Value("b0110".U)
  val SEL =   Value("b0111".U)
  val DOF =   Value("b1000".U)
  val FDOF =  Value("b1001".U)
  val VEC =   Value("b1100".U)
  val WIDTH = Value("b1111".U)
}

/**
 * Defines the S-type load/store flags
 */
object StypeLoadStore extends ChiselEnum {
  val LOAD = Value(0.U)
  val STORE = Value(1.U)
}

/**
 * Defines values that make decoding base memory addresses for the various memory regions easier. The actual decoding
 * is performed in hardware
 */
object StypeBaseAddress extends ChiselEnum {
  val X = Value(0.U)
  val XPHYS = Value(1.U)
  val XNEW = Value(2.U)
  val DC = Value(3.U)
  val DV = Value(4.U)
  val F = Value(5.U)
  val U = Value(6.U)
  val R = Value(7.U)
  val Z = Value(8.U)
  val P = Value(9.U)
  val Q = Value(10.U)
  val INVD = Value(11.U)
  val TMP = Value(12.U)
  val WIDTH = Value("b111111".U)
}



/**
 * Defines O-type modifiers
 */
object OtypeMod extends ChiselEnum {
  val PACKET = Value(1.U)
  val EXEC = Value(2.U)
  val TIME = Value(4.U)
  val WIDTH = Value("b1111".U)
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
  val SINGLE = Value(2.U)
  val NELEMVEC = Value(4.U)
  val NELEMDOF = Value(5.U)
  val NELEMSTEP = Value(6.U)
  val WIDTH = Value("b111111".U)
}

object BranchComp extends ChiselEnum {
  val EQUAL = Value("b000000".U)
  val NEQ = Value("b000001".U)
  val LT = Value("b000010".U)
  val GEQ = Value("b000011".U)
  val WIDTH = Value("b111111".U)
}


/**
 * Opcodes supported by a processing element.
 */
object Opcode extends ChiselEnum {
  val NOP = Value("b000000".U)
  val ADD = Value("b000100".U)
  val SUB = Value("b000101".U)
  val MAX = Value("b000110".U)
  val MIN = Value("b000111".U)
  val ABS = Value("b001000".U)
  val MUL = Value("b010000".U)
  val MAC = Value("b010001".U)
  val RED = Value("b010011".U)
  val DIV = Value("b100000".U)
}

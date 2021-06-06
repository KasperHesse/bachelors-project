package utils

import java.io.{BufferedWriter, FileWriter}
import java.util.NoSuchElementException
import chisel3._
import pipeline.InstructionFMT._
import pipeline.{Opcode, OtypeInstruction, _}
import utils.Config._
import utils.Fixed._

import scala.io.Source
import scala.language.implicitConversions

//import scala.collection.mutable.ListBuffer
import scala.collection.mutable._
import scala.collection.mutable

/**
 * This object is the singleton object used to invoke the assembler to generate programming files for the topological optimizer.
 */
object Assembler {
  import LitVals._

  def main(args: Array[String]): Unit = {
    val p = "istart\n" +
      "estart\n" +
      "mvp vs0, vs1 \n" +
      "mul.xv vs0, x1, vs2\n" +
      "eend\n" +
      "iend"

    val code = assemble(p);
    val z = "00000000000000000000000000000000"
    println("                             mod")
    println("                 rd   rs2 rs1 | fmt        op")
    println("                 |    |    |  | |           |")
    println("                 v    v    v  v v           v")
    code.foreach(a =>
      print(s"0x${(z + a.toHexString).takeRight(8)} / ${(z + a.toBinaryString).takeRight(32)}\n")
    )
  }

  /**
   * Initializes a memory file
   * @param memfile Relative path to the memory file to initialize. Existing contents are overwritten, a new file is created if none exists
   * @param instrs Encoded instruction to write into that file
   * @param len The number of figures to print out for each line (if eg value=8 and len=4, it will print 0004)
   */
  def writeMemInitFile(memfile: String, instrs: Array[Long], len: Int = 8): Unit = {
    val writer = new BufferedWriter(new FileWriter(memfile))
    for(instr <- instrs) {
      writer.write(("0000000000000000" + instr.toHexString).takeRight(len) + "\n")
    }
    writer.close()
    println(s"\tWrote ${instrs.length} instructions to $memfile")
  }

  /**
   * Implicit conversion of scala boolean to integer
   * @param b The boolean to convert
   * @return 1 if b==true, 0 otherwise
   */
  implicit def bool2int(b: Boolean): Int = if(b) 1 else 0

  /**
   * Parses a string representing a vector register identifier, returning the value of the register if valid
   * @param v The string represeting the vector register
   * @return The value of register
   */
  def vReg(v: String): Int = {
    require(v.startsWith("v"), "Vector slot indices must start with 'v'")
    val i = v.substring(1).toInt
    require(i >= 0 && i < NUM_VREG_SLOTS, s"Vector slot indices must be between 0 and ${NUM_VREG_SLOTS-1} (got $i)")
    i
  }

  def sReg(v: String): Int = {
    require(v.startsWith("s"), "Scalar registers must be prefixed with 's'")
    val i = v.substring(1).toInt
    require(i >= 0 && i < NUM_SREG, s"Scalar register indices must be between 0 and ${NUM_SREG-1} (got $i)")
    i
  }

  def xReg(v: String): Int = {
    require(v.startsWith("x"), s"X-registers must be prefixed with 'x', got ${v.substring(0,1)}")
    val i = v.substring(1).toInt
    require(i >= 0 && i < NUM_XREG, s"X-register indices must be between 0 and ${NUM_XREG-1} (got $i)")
    i
  }

  /**
   * Parses a field containing an Rtype-opcode, returning the correct Opcode
   * @param opcode The string containing the opcode (in format opcode.mod)
   * @return The integer value of that opcode
   */
  def opcode(opcode: String): Int = {
    val op = opcode.split("\\.")(0) //Split at "." and take the first value
    op match {
      case "add" => ADD
      case "sub" => SUB
      case "mul" => MUL
      case "div" => DIV
      case "mac" => MAC
      case "max" => MAX
      case "min" => MIN
      case "abs" => ABS
      case "red" => RED
      case _ => throw new IllegalArgumentException(s"Opcode '$op' not recognized")
    }
  }

  /**
   * Parses the field containing the Rtype-modifier, returning the correct RtypeMod
   * @param rmod The string containing the modifier (in format opcode.mod)
   * @return The enum representing the correct modifier
   */
  def rMod(rmod: String): Int = {
    val mod = rmod.split("\\.")(1) //Split at the period, take the second operand

    mod match {
      case "xx" => XX
      case "xv" => XV
      case "ss" => SS
      case "sv" => SV
      case "vv" => VV
      case "sx" => SX
      case "kv" => KV
      case "iv" => SV
      case "ix" => SX
      case "is" => SS
      case _ => throw new IllegalArgumentException(s"R-type modifier '$mod' not recognized")
    }
  }

  /**
   * Parses the field containing the R-type modifier, returning the immediate flag value
   * @param rmod The string containing the modifier (in format opcode.mod)
   * @return The immediate flag value
   */
  def immFlag(rmod: String): Boolean = {
    val mod = rmod.split("\\.")(1) //Split at the period, take the second operand
    mod match {
      case "iv" | "ix" | "is" => IMM
      case _ => NOIMM
    }
  }

  /**
   * Parses the field containing an immediate value, returning the integer and fractional part of that immediate
   * @param imms The immediate value from the instruction
   * @return An array holding two values. At (0) is the bit pattern for integer part of the instruction.
   *         At (1) is the bit pattern for the fractional part
   */
  def immFrac(imms: String): Array[Int] = {
    val immd = try {
      imms.toDouble
    } catch {
      case e: NumberFormatException => throw new NumberFormatException(s"Unable to parse '$imms' as a double")
    }
    val immFixed = imm2fixed(imms.toDouble)
    fixedImm2parts(immFixed)
  }

  /**
   * Parses an Rtype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseRtype(tokens: Array[String]): Int = {
    import RtypeInstruction._

    val op = opcode(tokens(0))
    val mod = rMod(tokens(0))

    if(op == MAC && !Seq(KV, SV, VV).contains(mod)) {
      throw new IllegalArgumentException("MAC instructions can only be executed with modifiers 'kv', 'sv', and 'vv'")
    }
    if(mod == KV && op != MAC) {
      throw new IllegalArgumentException("KV instructions can only have the MAC opcode")
    }

    val fmt = RTYPE
    val immflag = immFlag(tokens(0))
    val imm = if(immflag) immFrac(tokens(3)) else Array(0,0) //Only perform this if immflag is set

    val immint = imm(0)
    val immfrac = imm(1)

    val rd = if(op == MAC) mod match {
      case VV  | SV => sReg(tokens(1))
      case KV => vReg(tokens(1))
      case _ => throw new IllegalArgumentException("MAC instructions can only be executed with modifiers 'kv', 'sv', and 'vv'")
    } else mod match {
      case VV | XV | SV | KV => vReg(tokens(1))
      case SS => sReg(tokens(1))
      case XX | SX => xReg(tokens(1))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rd field")
    }

    val rs1 = if(immflag) mod match {
      case SV => vReg(tokens(2))
      case SX => xReg(tokens(2))
      case SS => sReg(tokens(2))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs1 field in immediate instruction")
    } else mod match {
      case VV | KV => vReg(tokens(2))
      case XX | XV => xReg(tokens(2))
      case SS | SV | SX => sReg(tokens(2))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs1 field")
    }

    val rs2 = if(immflag) immint else mod match {
      case VV | SV | XV => vReg(tokens(3))
      case SS => sReg(tokens(3))
      case XX | SX => xReg(tokens(3))
      case KV => 0 //MVP instructions don't use rs2 to anything
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs2 field")
    }

    //Build the instruction
    var instr: Int = 0
    instr |= op << OP_OFFSET
    instr |= fmt << FMT_OFFSET
    instr |= mod << MOD_OFFSET
    instr |= immflag << IMMFLAG_OFFSET
    instr |= rd << RD_OFFSET
    instr |= rs1 << RS1_OFFSET
    instr |= rs2 << RS2_OFFSET
    instr |= immfrac << FRAC_OFFSET

    instr
  }
  /**
   * Parses an Otype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseOtype(tokens: Array[String], incrementType: Array[Int] = Array(0)): Int = {
    import OtypeInstruction._

    def startEndFlag(str: String): Int = {
      if(str.substring(1).equals("start")) {
        START
      } else if (str.substring(1).equals("end")) {
        END
      } else {
        throw new IllegalArgumentException("O-type instructions must have 'start' or 'end' in their name")
      }
    }

    def packetExecFlag(str: String): Int = {
      if(str.substring(0,1).equals("p")) {
        PACKET
      } else if (str.substring(0,1).equals("e")) {
        EXEC
      } else {
        throw new IllegalArgumentException("O-type instruction must start with 'p' or 'e'")
      }
    }

    def length(str: String): Int = {
      val map = Map("single" -> SINGLE,
      "ndof" -> NDOF,
      "nelemvec" -> NELEMVEC,
      "nelemdof" -> NELEMDOF,
      "nelemstep" -> NELEMSTEP)
      if(map.contains(str)) map(str) else throw new IllegalArgumentException(s"Unable to parse instruction length $str")
    }
    //Parse i/e and start/end values
    val se = startEndFlag(tokens(0))
    val pe = packetExecFlag(tokens(0))

    val len = if (se == START && pe == PACKET) length(tokens(1)) else SINGLE //SINGLE is used as a placeholder value in estart, eend and pend instructions
    incrementType(0) = len
    val fmt = OTYPE

    var instr: Int = 0
    instr |= se << SE_OFFSET
    instr |= pe << PE_OFFSET
    instr |= len << LEN_OFFSET
    instr |= fmt << FMT_OFFSET

    instr
  }

  /**
   * Extracts the S-type modifier from an instruction, or throws an error if no valid modifier is present
   * @param token The first token parsed, of the type [loadstore].[mod]
   * @return An integer represnting that modifier
   */
  def sMod(mod: String): Int = {
    val map = Map("vec" -> VEC,
      "dof" -> DOF,
      "fdof" -> FDOF,
      "fcn" -> FCN,
      "edn1" -> EDN1,
      "edn2" -> EDN2,
      "sel" -> SEL,
      "elem" -> ELEM)
    if (map.contains(mod)) map(mod) else throw new IllegalArgumentException(s"Unable to recognize Stype-modifier $mod")
  }

  def sLoadStore(ls: String): Int = {
    val map = Map("ld" -> LOAD,
    "st" -> STORE)
    if(map.contains(ls)) map(ls) else throw new IllegalArgumentException(s"Unable to recognize Stype load/store $ls")
  }

  def sBaseAddr(baseAddr: String): Int = {
    val map = Map("x" -> X,
    "xphys" -> XPHYS,
    "xnew" -> XNEW,
    "dc" -> DC,
    "dv" -> DV,
    "f" -> F,
    "u" -> U,
    "r" -> R,
    "z" -> Z,
    "p" -> P,
    "q" -> Q,
    "invd" -> INVD,
    "tmp" -> TMP)
    if(map.contains(baseAddr)) map(baseAddr) else throw new IllegalArgumentException(s"Unable to recognize Stype base address $baseAddr")
  }

  /**
   * Parses an Stype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseStype(tokens: Array[String], incrementType: Array[Int]): Int = {
    import StypeInstruction._
    require(tokens.length == 3, "S-type instructions must have exactly 3 tokens")

    val lsString = tokens(0).split("\\.")(0)
    val modString = tokens(0).split("\\.")(1)
    val rsrdString = tokens(1)
    val baseAddrString = tokens(2)

    val ls = sLoadStore(lsString)
    val mod = sMod(modString)
    val rsrd = if(Seq(DOF,FDOF,VEC).contains(mod)) vReg(rsrdString) else xReg(rsrdString)
    val baseAddr = sBaseAddr(baseAddrString)
    val it = incrementType(0)

    //Perform compatibility checks
    if(ls == STORE && Seq(FCN,EDN1,EDN2).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform store operations with Stype modifier $modString")
    } else if (ls == LOAD && mod == FDOF) {
      throw new IllegalArgumentException(s"Cannot perform load operations with Stype modifier $modString")
    } else if (Seq(DOF, FDOF).contains(mod) && Seq(X, XPHYS, XNEW, DC, DV).contains(baseAddr)) {
      throw new IllegalArgumentException(s"Cannot perform dof/fdof operations to base address $baseAddrString")
    } else if (Seq(ELEM, SEL, FCN, EDN1, EDN2).contains(mod) && !Seq(X, XPHYS, XNEW, DC, DV).contains(baseAddr)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations to base address $baseAddrString")
    } else if (it == NELEMSTEP && !Seq(SEL, FCN, EDN1, EDN2).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations when increment type is 'nelemstep'")
    } else if (it == NELEMDOF && !Seq(DOF, ELEM, FDOF).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations when increment type is 'nelemdof'")
    } else if (Seq(NELEMVEC, NDOF).contains(it) && mod != VEC) {
      throw new IllegalArgumentException(s"Can only perform ld.vec and st.vec operations when increment type is 'nelemvec' or 'ndof'")
    }

    //TODO add error checking for dof/fdof loads on non-ndof long vectors and elem loads on ndof-long vectors

    val fmt = STYPE
    //Generate instruction
    var instr: Int = 0
    instr |= ls << LS_OFFSET
    instr |= mod << MOD_OFFSET
    instr |= fmt << FMT_OFFSET
    instr |= rsrd << RSRD_OFFSET
    instr |= baseAddr << BASEADDR_OFFSET
    instr
  }

  def parsePseudoInstruction(tokens: Array[String]): Int = {
    throw new IllegalArgumentException("Pseudo-instruction decode is not support yet.")
  }

  /**
   * Parses an Btype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseBtype(tokens: Array[String], symbols: mutable.Map[String, Int], pc: Int): Int = {
    import BtypeInstruction._

    /**
     * Parses the field containg the comparison string, returning the correct bit pattern for the comparison
     * @param comp The string containing the comparison field
     * @return The bit pattern representing that comparison
     * @throws IllegalArgumentException if the parameter string does not match any valid comparison
     */
    def comparison(comp: String): Int = {
      if(comp.equals("beq")) {
        EQUAL
      } else if (comp.equals("bne")) {
        NEQ
      } else if (comp.equals("blt")) {
        LT
      } else if (comp.equals("bge")) {
        GEQ
      } else {
        throw new IllegalArgumentException(s"Unrecognized branch string '$comp''")
      }
    }

    /**
     * Parses the offset field of a branch instruction, returning the two bit patterns representing the offset
     * @param offsets A string representing the branch offset
     * @return An array holding at (0) the high bits of the branch target and at (1) the low bits
     */
    def offset2targets(offsets: String): Array[Int] = {
      val offset = offsets.toInt
      require(offset % 4 == 0, "Branch offset must be a multiple of 4")
      val targetl = (offset >> 2) & ((1 << TARGETL_WIDTH)-1)
      val targeth = (offset >> 2 + TARGETL_WIDTH) & ((1 << TARGETH_WIDTH)-1)
      Array(targeth, targetl)
    }

    /**
     * Parses a named branch where the branch target is a label and not a direct reference to the branch amount
     * @return An array holding at (0) the high bits of the branc htarget and at (1) the low bits
     */
    def namedBranch(): Array[Int] = {
      val target = symbols(tokens(3))
      val diff = target-pc
      offset2targets(diff.toString)
    }

    val comp = comparison(tokens(0))
    val rs1 = sReg(tokens(1))
    val rs2 = sReg(tokens(2))
    val target = if(symbols.contains(tokens(3))) namedBranch() else throw new IllegalArgumentException(s"Unrecognized label '${tokens(3)}'")
    val fmt = BTYPE

    var instr = 0
    instr |= comp << COMP_OFFSET
    instr |= fmt << FMT_OFFSET
    instr |= target(1) << TARGETL_OFFSET
    instr |= rs1 << RS1_OFFSET
    instr |= rs2 << RS2_OFFSET
    instr |= target(0) << TARGETH_OFFSET

    instr
  }

  /**
   * Splits an instruction into its constituent parts by splitting at all spaces and commas
   * @param line The line to split
   * @return The tokens making up that string
   */
  def split(line: String): Array[String] = {
    line.split(",? +") //0 or 1 commas, followed by any number of spaces
  }

  /**
   * Parses a symbol for the symbol table
   * @param symbols The current symbol table
   * @param pc The current program counter
   * @param tokens The tokens for this line
   */
  def parseSymbol(symbols: mutable.Map[String, Int], pc: Int, tokens: Array[String]): Unit = {
    val key = tokens(0).dropRight(1)
    if(tokens.length == 1) {
      if(symbols.contains(key)) {
        throw new IllegalArgumentException(s"Duplicate label '$key' already exists, mapping to PC ${symbols(key)}'")
      }
      symbols += (key -> pc)
    } else {
      throw new IllegalArgumentException("Labels must be on their own line above the instruction they refer to")
    }
  }

  def assemble(source: Source): Array[Int] = {
    val lines = source.mkString
    assemble(lines)
  }

  /**
   * Assembles a program for the topological optimizer. Currently has limited error checking
   *
   * @param program A string holding the program to be assembled
   * @return An array containing the instructions
   */
  def assemble(program: String): Array[Int] = {
    val prog = program.toLowerCase
    val lines: Array[String] = prog.trim.split("[\r\n]+") //Split at newlines

    var pc: Int = 0
    val symbols: scala.collection.mutable.Map[String, Int] = Map[String, Int]()

    //TODO Probably need to spot pseudo instructions all the way up here

    var pstart = false
    var estart = false
    var eend = false
    var pend = false
    var packetSize = 0
    var mac = false
    val incrementType: Array[Int] = Array(0) //Using an array to allow modification and pass by reference

    val code = ListBuffer.empty[Int]
    assemblerPass(false)
    code.clear()
    pc = 0
    assemblerPass(true)

    def assemblerPass(pass2: Boolean): Unit = {
      for(i <- lines.indices) {
        val line = lines(i).trim
        //Detect pseudo-instructions here and perform substitutions?
        val tokens = split(line)
        val symbolPattern = "([\\w-]+:)".r//Any characters, followed by a :
        try {
          performErrorChecks(tokens(0))

          val instr = tokens(0) match {
            case x if x.startsWith("//") => "" //Comment
            case x if x.trim().equals("") => "" //Blank line
            case symbolPattern(x) => if(!pass2) parseSymbol(symbols, pc, tokens)
            case "pstart" => parseOtype(tokens, incrementType)
            case "estart" => parseOtype(tokens, incrementType)
            case "eend" => parseOtype(tokens, incrementType)
            case "pend" => parseOtype(tokens, incrementType)

            case x if x.startsWith("st.") => parseStype(tokens, incrementType)
            case x if x.startsWith("ld.") => parseStype(tokens, incrementType)

            case x if x.startsWith("mvp") => parsePseudoInstruction(tokens)
            case x if x.startsWith("sqrt") => parsePseudoInstruction(tokens)

            case x if x.startsWith("add.") => parseRtype(tokens)
            case x if x.startsWith("sub.") => parseRtype(tokens)
            case x if x.startsWith("mul.") => parseRtype(tokens)
            case x if x.startsWith("div.") => parseRtype(tokens)
            case x if x.startsWith("mac.") => parseRtype(tokens)
            case x if x.startsWith("max.") => parseRtype(tokens)
            case x if x.startsWith("min.") => parseRtype(tokens)
            case x if x.startsWith("abs.") => parseRtype(tokens)
            case x if x.startsWith("red.") => parseRtype(tokens)

            case x if x.startsWith("beq") => if(pass2) parseBtype(tokens, symbols, pc) else 0
            case x if x.startsWith("bne") => if(pass2) parseBtype(tokens, symbols, pc) else 0
            case x if x.startsWith("blt") => if(pass2) parseBtype(tokens, symbols, pc) else 0
            case x if x.startsWith("bge") => if(pass2) parseBtype(tokens, symbols, pc) else 0

            case _ => throw new IllegalArgumentException(s"'${tokens(0)}' was not recognized as a valid instruction")
          }

          instr match {
            case a: Int => {
              code += a
              if(pstart) packetSize += 1
              pc += 4
            } //Add instruction to list of instructions
            case a: String => //Empty string is given here on comments or blank lines
            case _ => // Something else
          }
        } catch {
          case e: Exception => throw new IllegalArgumentException(s"Error at line $i ($line): ${e.getMessage}")
        }
      }
    }



    /**
     * Performs error checking to ensure that the given instruction was placed in an allowed position
     * @param op The first token in the instruction
     */
    def performErrorChecks(op: String): Unit = {
      op match {
        case "pstart" => if(pstart) {
          throw new IllegalArgumentException("Cannot have nested pstart instructions")
        } else {
          pstart = true
          packetSize = 1
        }
        case "estart" => if(!pstart) {
          throw new IllegalArgumentException("Estart must be preceded by a pstart instruction")
        } else if (estart) {
          throw new IllegalArgumentException("Cannot have nested estart instructions")
        } else {
          estart = true
        }
        case "eend" => if(!pstart || !estart) {
          throw new IllegalArgumentException("eend must be preceded by pstart end estart")
        } else {
          eend = true
        }
        case "pend" => if(!eend) {
          throw new IllegalArgumentException("pend must be preceded by eend")
        } else {
          pstart = false
          estart = false
          eend = false
          mac = false
          if(packetSize > INSTRUCTION_BUFFER_SIZE) {
            throw new IllegalArgumentException(s"Instruction packet too large. Maximum size is $INSTRUCTION_BUFFER_SIZE, this packet is $packetSize instructions long")
          }
          packetSize = 0
        }
        case "//" => return
        case x: String =>
          if(Seq("add","sub", "mul", "div", "max", "min", "mac").contains(x.substring(0,3)) && (!estart || eend)) {
            throw new IllegalArgumentException("R-type instructions only allowed between estart and eend")
          } else if (x.startsWith("ld") && (!pstart || estart)) {
            throw new IllegalArgumentException("Load instructions only allowed between pstart and estart")
          } else if (x.startsWith("st") && !eend) {
            throw new IllegalArgumentException("Store instructions only allowed between eend and pend")
          } else if(Seq("beq", "bne", "blt", "bge").contains(x) && pstart) {
            throw new IllegalArgumentException("Branch instructions cannot be inside an instruction packet")
          }
          if(x.contains("mac") && mac) {
            throw new IllegalArgumentException("There can only be one mac instruction in each instruction packet")
          } else if(x.contains("mac")) {
            mac = true
          }
      } //TODO check packetsize

    }
    code.toArray[Int]
  }
}

/**
 * Literal values for instruction encoding
 */
object LitVals {
  //Instruction formats
  val RTYPE = 0x0
  val STYPE = 0x1
  val OTYPE = 0x2
  val BTYPE = 0x3

  //Opcodes
  val ADD = 0x04 //000100
  val SUB = 0x05 //000101
  val MAX = 0x06 //000110
  val MIN = 0x07 //000111
  val ABS = 0x08 //001000
  val MUL = 0x10 //010000
  val MAC = 0x11 //010001
  val RED = 0x13 //010011
  val DIV = 0x20 //100000

  //R-type modifiers
  val VV = 0x0
  val XV = 0x4
  val XX = 0x5
  val SV = 0x8
  val SX = 0x9
  val SS = 0xa
  val KV = 0xc

  //R-type immediate flag
  val IMM = true
  val NOIMM = false

  //S-type load/store flag
  val LOAD = 0x0
  val STORE = 0x3

  //S-type modifier
  val ELEM = 0x2
  val EDN1 = 0x4
  val EDN2 = 0x5
  val FCN  = 0x6
  val SEL = 0x7
  val DOF  = 0x8
  val FDOF = 0x9
  val VEC = 0xc

  //S-type base address
  val X = 0
  val XPHYS = 1
  val XNEW = 2
  val DC = 3
  val DV = 4
  val F = 5
  val U = 6
  val R = 7
  val Z = 8
  val P = 9
  val Q = 10
  val INVD = 11
  val TMP = 12

  //O-type modifier
  val PACKET = 0x1
  val EXEC = 0x2

  //O-type SE
  val END = 0x0
  val START = 0x1

  //O-type instruction length
  val NDOF = 0x0
  val SINGLE = 0x2
  val NELEMVEC = 0x4
  val NELEMDOF = 0x5
  val NELEMSTEP = 0x6

  //O-type increment type
  val LINEAR = 0x0
  val IJK = 0x1

  //B-type comparisons
  val EQUAL = 0x0
  val NEQ = 0x1
  val LT = 0x2
  val GEQ = 0x3
}
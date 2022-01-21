package utils

import java.io.{BufferedWriter, File, FileWriter}
import execution.{Opcode, OtypeInstruction, _}
import utils.Config._
import utils.Fixed._

import scala.io.Source
import scala.language.implicitConversions
import scala.util.matching.Regex

import scala.collection.mutable._
import scala.collection.mutable

/**
 * This object is the singleton object used to invoke the assembler to generate programming files for the topological optimizer.
 */
object Assembler {
  import LitVals._

  var pstart: Boolean = false
  var estart: Boolean = false
  var eend: Boolean = false
  var pend: Boolean = false
  var mac: Boolean = false
  var instructionLength: Int = 0
  var packetSize: Int = 0

  var symbols: mutable.Map[String, Int] = mutable.Map()
  /** Functions defined in the currently assembled file */
  var functions: mutable.Map[String, AssemblerFunctionCall] = mutable.Map()
  var pc: Int = 0
  var code: ListBuffer[Long] = ListBuffer.empty[Long]

  val symbolRegex: Regex = "([\\w]+:)".r//Any characters, followed by a :
  val functionRegex: Regex = "func (\\w+) *\\(((?:\\w+)*(?: *, *\\w+)*)\\) *= *\\{\\s+([^}]*)[\\n\\r]+\\s*\\}".r

  /**
   * Resets the state of the assembler, resetting all member variables
   */
  def resetState(): Unit = {
    pstart = false
    estart = false
    eend = false
    pend = false
    mac = false
    this.instructionLength = 0
    packetSize = 0
    pc = 0

    symbols.clear()
    functions.clear()
    code.clear()
  }


  /**
   * Initializes a memory file
   * @param memfile Relative path to the memory file to initialize. Existing contents are overwritten, a new file is created if none exists
   * @param data The data to write into that file
   * @param len The number of figures to print out for each line (if eg value=8 and len=4, it will print 0008)
   */
  def writeMemInitFile(memfile: String, data: Array[Long], len: Int = 16): Unit = {
    val dir = new File(new File(memfile).getParent)
    if(!dir.exists) {
      dir.mkdirs()
    }
    val zeros = "0" * len
    val writer = new BufferedWriter(new FileWriter(memfile))
    for(instr <- data) {
      writer.write((zeros + instr.toHexString).takeRight(len) + "\n")
    }
    writer.close()
    println(s"\tWrote ${data.length} lines to $memfile")
  }

  /**
   * Initializes a memory file, generating a binary output file.
   * @param memfile Relative path to the memory file to initialize. Existing contents are overwritten, a new file is created if none exists
   * @param data The data to write into that file
   * @param len The number of figures to print out for each line (if eg value=5 and len=4, it will print 0101)
   */
  def writeMemInitFileBinary(memfile: String, data: Array[Long], len: Int = 54): Unit = {
    val dir = new File(new File(memfile).getParent)
    if(!dir.exists) {
      dir.mkdirs()
    }
    val zeros = "0" * len
    val writer = new BufferedWriter(new FileWriter(memfile))
    for(d <- data) {
      writer.write((zeros + d.toBinaryString).takeRight(len) + "\n")
    }
    writer.close()
    println(s"\tWrote ${data.length} lines to $memfile")
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
      case "nez" => NEZ
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
    val s = rmod.split("\\.") //Split at the period
    if(s.length == 1) {
      throw new IllegalArgumentException("Did not find an R-type modifier")
    }
    val mod = s(1)

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
    val immFixed = imm2long(immd)
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

    if(op == MAC && !mutable.Seq(KV, SV, VV).contains(mod)) {
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
    } else if (op == RED) mod match {
      case VV => xReg(tokens(1))
      case XX => sReg(tokens(1))
      case _ => throw new IllegalArgumentException(s"RED instructions cannot be executed with modifier ${tokens(0).split(".").last}, only with .vv and .xx")
    } else mod match {
      case VV | XV | SV | KV => vReg(tokens(1))
      case SS => sReg(tokens(1))
      case XX | SX => xReg(tokens(1))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rd field")
    }

    val rs1 = if(immflag) immint else mod match {
      case VV | KV => vReg(tokens(2))
      case XX | XV => xReg(tokens(2))
      case SS | SV | SX => sReg(tokens(2))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs1 field in immediate instruction")
    }

    val rs2 = if(immflag) mod match {
      case SV => vReg(tokens(2))
      case SX => xReg(tokens(2))
      case SS => sReg(tokens(2))
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs2 field")
    } else if (op != ABS && op != NEZ) mod match {
      case VV | SV | XV => vReg(tokens(3))
      case SS => sReg(tokens(3))
      case XX | SX => xReg(tokens(3))
      case KV => 0 //MVP instructions don't use rs2
      case _ => throw new IllegalArgumentException(s"Unrecognized modifier '$mod' for setting rs2 field")
    } else {
      if(tokens.length > 3 && !tokens(3).startsWith("//")) {
        throw new IllegalArgumentException("ABS and NEZ instructions do not take a second source register as operands")
      }
      0 //ABS, NEZ instructions only take one operand
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
  def parseOtype(tokens: Array[String]): Int = {
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

    def modifier(str: String): Int = {
      val str2 = str.substring(0,1)
      val map = mutable.Map("p" -> PACKET,
      "e" -> EXEC,
      "t" -> TIME)
      if(map.contains(str2)) map(str2) else throw new IllegalArgumentException(s"Unable to parse O-type modifier $str2")
    }

    def length(str: String): Int = {
      val map = mutable.Map("single" -> SINGLE,
      "double" -> DOUBLE,
      "ndof" -> NDOF,
      "nelemvec" -> NELEMVEC,
      "nelemdof" -> NELEMDOF,
      "nelemstep" -> NELEMSTEP,
      "clear" -> CLEAR, //Clear, run are only used for tstart instructions
      "run" -> RUN)
      if(map.contains(str)) map(str) else throw new IllegalArgumentException(s"Unable to parse instruction length $str")
    }
    //Parse mod and start/end values
    val se = startEndFlag(tokens(0))
    val mod = modifier(tokens(0))

    val len = if (se == START && (mod == PACKET || mod == TIME)) length(tokens(1)) else SINGLE //SINGLE is used as a placeholder value in estart, eend and pend instructions
    if (se == START && mod == PACKET) this.instructionLength = len
    val fmt = OTYPE

    var instr: Int = 0
    instr |= se << SE_OFFSET
    instr |= mod << MOD_OFFSET
    instr |= len << LEN_OFFSET
    instr |= fmt << FMT_OFFSET

    instr
  }

  /**
   * Extracts the S-type modifier from an instruction, or throws an error if no valid modifier is present
   * @param mod The first token parsed, of the type [loadstore].[mod]
   * @return An integer represnting that modifier
   */
  def sMod(mod: String): Int = {
    val map = mutable.Map(
      "vec" -> VEC,
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
    val map = mutable.Map("ld" -> LOAD,
    "st" -> STORE)
    if(map.contains(ls)) map(ls) else throw new IllegalArgumentException(s"Unable to recognize Stype load/store $ls")
  }

  def sBaseAddr(baseAddr: String): Int = {
    val map = mutable.Map(
      "x" -> X,
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
      "tmp" -> TMP,
      "uart" -> UART)
    if(map.contains(baseAddr)) map(baseAddr) else throw new IllegalArgumentException(s"Unable to recognize Stype base address $baseAddr")
  }

  /**
   * Parses an Stype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseStype(tokens: Array[String]): Int = {
    import StypeInstruction._

    val lsString = tokens(0).split("\\.")(0)
    val modString = tokens(0).split("\\.")(1)
    val rsrdString = tokens(1)
    val baseAddrString = tokens(2)

    val ls = sLoadStore(lsString)
    val mod = sMod(modString)
    val rsrd = if(mutable.Seq(DOF,FDOF,VEC).contains(mod)) vReg(rsrdString) else xReg(rsrdString)
    val baseAddr = sBaseAddr(baseAddrString)

    //Perform compatibility checks
    if(ls == STORE && mutable.Seq(FCN,EDN1,EDN2).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform store operations with Stype modifier $modString")
    } else if (ls == LOAD && mod == FDOF) {
      throw new IllegalArgumentException(s"Cannot perform load operations with Stype modifier $modString")
    } else if (mutable.Seq(DOF, FDOF).contains(mod) && mutable.Seq(X, XPHYS, XNEW, DC, DV).contains(baseAddr)) {
      throw new IllegalArgumentException(s"Cannot perform dof/fdof operations to base address $baseAddrString")
    } else if (mutable.Seq(ELEM, SEL, FCN, EDN1, EDN2).contains(mod) && !mutable.Seq(X, XPHYS, XNEW, DC, DV, TMP, UART).contains(baseAddr)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations to base address $baseAddrString")
    } else if (this.instructionLength == NELEMSTEP && !mutable.Seq(SEL, FCN, EDN1, EDN2).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations when increment type is 'nelemstep'")
    } else if (this.instructionLength == NELEMDOF && !mutable.Seq(DOF, ELEM, FDOF).contains(mod)) {
      throw new IllegalArgumentException(s"Cannot perform $modString operations when increment type is 'nelemdof'")
    } else if (mod == VEC && !mutable.Seq(NELEMVEC, NDOF, SINGLE).contains(this.instructionLength)) {
      throw new IllegalArgumentException(s"Can only perform ld.vec and st.vec operations when increment type is 'nelemvec' or 'ndof'")
    } else if (baseAddr == UART && mod != VEC && baseAddr != UART) {
      throw new IllegalArgumentException(s"Can only access the uart with st.vec operations. Operation was ${tokens(0)}")
    }

    val fmt = STYPE
    //Generate instruction
    var instr: Int = 0
    instr |= baseAddr << BASEADDR_OFFSET
    instr |= fmt << FMT_OFFSET
    instr |= mod << MOD_OFFSET
    instr |= ls << LS_OFFSET
    instr |= rsrd << RSRD_OFFSET

    instr
  }

  /**
   * Parses an Btype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseBtype(tokens: Array[String]): Int = {
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
  def splitInstruction(line: String): Array[String] = {
    line.split(",?\\s+") //0 or 1 commas, followed by any amount of whitespace
  }

  /**
   * Parses a symbol for the symbol table
   * @param tokens The tokens for this line
   */
  def parseSymbol(tokens: Array[String]): Unit = {
    val key = tokens(0).dropRight(1)
    if(tokens.length == 1) {
      if(symbols.contains(key)) {
        throw new IllegalArgumentException(s"Duplicate label '$key' already exists, mapping to PC ${symbols(key)}")
      }
      symbols += (key -> pc)
    } else {
      throw new IllegalArgumentException("Labels must be on their own line above the instruction they refer to")
    }
  }

  /**
   * Assembles a program for the topological optimizer.
   * @param source The source of the program to assemble. Usually a file handle opened with [[Source]][[Source#fromFile]]
   * @return An array containing the instructions
   */
  def assemble(source: Source): Array[Long] = {
    val lines = source.mkString
    assemble(lines)
  }

  /**
   * Assembles a program for the topological optimizer.
   *
   * @param program A string holding the program to be assembled
   * @return An array containing the instructions
   */
  def assemble(program: String): Array[Long] = {
    //Always reset pstart, estart etc once a new assembly operation is started
    this.resetState()
    //Get function definitions and strip out declarations
    val progFunctionsStripped = this.extractFunctionDefinitions(program.toLowerCase)
    val progFunctionsReplaced = this.replaceFunctionCalls(progFunctionsStripped)

    val lines = progFunctionsReplaced.trim.split("[\r\n]+") //Split at newlines

    assemblerPass(lines, false) //Spot symbols
    code.clear()
    pc = 0
    assemblerPass(lines, true) //Perform assembly

    code.toArray[Long]
  }

  /**
   * Performs a pass of the assembler through the program
   * @param lines The lines of the program to assemble
   * @param pass2 Whether this is the second assembly pass or not (true on second pass)
   */
  def assemblerPass(lines: Array[String], pass2: Boolean): Unit = {
    for(i <- lines.indices) {
      val line = lines(i).trim
      //Detect pseudo-instructions here and perform substitutions?
      val tokens = splitInstruction(line)
      try {
        performErrorChecks(tokens(0))
        val instr = matchTokens(tokens, pass2)

        instr match {
          case Some(a) => {
            code += a
            if(pstart) packetSize += 1
            pc += 4
          } //Add instruction to list of instructions
          case None => //Do nothing
        }
      } catch {
        case e: Exception => throw new IllegalArgumentException(s"Error at line $i ($line): ${e.getMessage}")
      }
    }
  }

  /**
   * Takes the tokens representing a single line of the program and returns the corresponding instruction
   * @param tokens The tokens representing one line of a program
   * @param pass2 Whether this is the first or second pass of the assembler
   * @return
   */
  def matchTokens(tokens: Array[String], pass2: Boolean): Option[Int] = {
    //All of these regexes must use a surrounding capture group to evaluate true in the match statement below
    val OType = "(?i)((?:pstart|estart|eend|pend|tstart|tend).*)".r
    val RType = "(?i)((?:add|sub|mul|div|mac|max|min|abs|nez||red)\\..+)".r
    val BType = "(?i)(beq|bne|blt|bge)".r
    val SType = "(?i)((?:st|ld)\\..+)".r

    tokens(0) match {
      case x if x.startsWith("//") => None //Comment
      case x if x.trim().equals("") => None //Blank line
      case symbolRegex(x) => if(!pass2) {parseSymbol(tokens); None} else None
      case OType(x) => Option(parseOtype(tokens))
      case RType(x) => Option(parseRtype(tokens))
      case SType(x) => Option(parseStype(tokens))
      case BType(x) => if(pass2) Option(parseBtype(tokens)) else Option(0)

      case _ => throw new IllegalArgumentException(s"'${tokens(0)}' was not recognized as a valid instruction")
    }
  }

  /**
   * Performs error checking to ensure that the given instruction was placed in an allowed position
   * @param token The first token in the instruction
   */
  def performErrorChecks(token: String): Unit = {
    token match {
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
      } else if(eend) {
        throw new IllegalArgumentException("Cannot have nested eend instructions")
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
      case x if x.startsWith("//") => return
      case "{" | "}" => throw new IllegalArgumentException("Floating curly brace. Curly braces should only be used in function definitions")
      case x: String =>
        if(mutable.Seq("add","sub", "mul", "div", "max", "min", "mac").contains(x.substring(0,3)) && (!estart || eend)) {
          throw new IllegalArgumentException("R-type instructions only allowed between estart and eend")
        } else if (x.startsWith("ld") && (!pstart || estart)) {
          throw new IllegalArgumentException("Load instructions only allowed between pstart and estart")
        } else if (x.startsWith("st") && !eend) {
          throw new IllegalArgumentException("Store instructions only allowed between eend and pend")
        } else if(mutable.Seq("beq", "bne", "blt", "bge").contains(x) && pstart) {
          throw new IllegalArgumentException("Branch instructions cannot be inside an instruction packet")
        }
        if(x.contains("mac") && mac) {
          throw new IllegalArgumentException("There can only be one mac instruction in each instruction packet")
        } else if(x.contains("mac")) {
          mac = true
        }
    }
  }

  /**
   * Extracts all function definitions (see [[AssemblerFunctionCall]] for a description of valid function syntax),
   * populating [[functions]] with a mapping of function names and function calls.
   * Returns a version of the program where all function definitions have been stripped
   * @param program
   * @return
   */
  def extractFunctionDefinitions(program: String): String = {
    val matches = this.functionRegex.findAllMatchIn(program) //Find all function call
    //Add them to list of functions. group(0) is entire matched text, group(1) is func name, group(2) is argument list and group(3) is body
    matches.foreach(m => functions(m.group(1)) = AssemblerFunctionCall(m.group(0)))
    program.replaceAll(this.functionRegex.regex, "").trim //Return original program without function defs, whitespace trimmed away
  }

  /**
   * Replaces all function calls in the code with their corresponding prototype. Returns a program where all function calls
   * have been removed
   * @param str The program with function calls removed
   * @return
   */
  def replaceFunctionCalls(str: String): String = {
    val functionCallRegex = "(\\w+)\\((.*)\\)".r
    //For all lines in the string, either perform function replacement, or just return that line.
    //Finally, recombine all lines back into a single string
//    functionCallRegex

    str.linesIterator.map {
      case functionCallRegex(name, args) => functions(name).invoke(args)
      case x => x
    }.foldLeft("")((a,b) => a + b + "\n")
  }
}


/**
 * A class representing an assembler function call
 * @param name
 * @param args
 * @param functionString
 */
class AssemblerFunctionCall(name: String, args: mutable.Seq[String], functionString: String) {

  /**
   * Invokes this assembler function call, causing it to output a string-replaced version of the prototype
   * @param replArgs The arguments used for replacement. Replacement is performed such that arg1 of the original string,
   *                 is replaced with replArgs[0], arg2 with replArgs[1], etc.
   * @return
   */
  def invoke(replArgs: mutable.Seq[String]): String = {
    require(this.args.length == replArgs.length, f"Function $name was called with ${replArgs.length} arguments (${replArgs.mkString(",")}), requires ${this.args.length} (${this.args.mkString(",")})")
    val map = (args, replArgs).zipped.toMap //Create mapping from original strings to their replacements
    map.foldLeft(functionString)((str, repl) => str.replaceAllLiterally(repl._1, repl._2)) //Apply all replacement to function call, returning modified version
  }

  def invoke(replArgs: String): String = {
    this.invoke(replArgs.split(',').map(a => a.trim))
  }
}

object AssemblerFunctionCall {
  val functionRegex: Regex = utils.Assembler.functionRegex

  private def apply(name: String, args: String, funcString: String): AssemblerFunctionCall = {
    this(name, args.split(',').map(s => s.trim), funcString)
  }

  private def apply(name: String, args: mutable.Seq[String], funcString: String): AssemblerFunctionCall = {
    new AssemblerFunctionCall(name, args, funcString)
  }

  /**
   * Takes an entire function string of the type
   * {{{
   * func functionName(arg1, arg2, ..., argN) = {
   *  contents
   * }
   * }}}
   * And creates an [[AssemblerFunctionCall]] representing that function
   * @param function The function string to be parsed
   * @return An object representing that function call which may be invoked by calling [[utils.AssemblerFunctionCall#invoke]]
   */
  def apply(function: String): AssemblerFunctionCall = {
    val trim = function.trim

    trim match {
      case functionRegex(name, args, content) => AssemblerFunctionCall(name, args, content)
      case _ => throw new IllegalArgumentException(f"Function string $trim did not match function template")
    }
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

//  import utils.AssemblerFunctionCall
//  val x = new AssemblerFunctionCall

  //Opcodes
  val ADD = 0x08 //001000
  val SUB = 0x09 //001001
  val MAX = 0x0A //001010
  val MIN = 0x0B //001011
  val ABS = 0x0C //001100
  val NEZ = 0x0D //001101
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
  val STORE = 0x1

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
  val UART = 13

  //O-type modifier
  val PACKET = 0x1
  val EXEC = 0x2
  val TIME = 0x4

  //O-type SE
  val END = 0x0
  val START = 0x1

  //O-type instruction length
  val NDOF = 0x0
  val SINGLE = 0x2
  val DOUBLE = 0x3
  val NELEMVEC = 0x4
  val NELEMDOF = 0x5
  val NELEMSTEP = 0x6

  //O-type timer code
  val CLEAR = 0x0
  val RUN = 0x2

  //O-type increment type
  val LINEAR = 0x0
  val IJK = 0x1

  //B-type comparisons
  val EQUAL = 0x0
  val NEQ = 0x1
  val LT = 0x2
  val GEQ = 0x3
}
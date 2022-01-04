import java.io.{BufferedWriter, FileWriter}
import chisel3._
import chiseltest._
import execution.KEWrapper
import utils.{Assembler, Config}
import utils.Config._
import utils.Fixed._
import execution.Opcode._
import org.scalatest.{FlatSpec, Matchers}

package object execution {
  /**
   * Generates a random Rtype-instruction (random mod+opcode)
   * @return
   */
  def genRtype(): RtypeInstruction = {
    import RtypeMod._
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)

//    if(rand.nextInt(10) == 9) { //Generate MVP instruction and exit 10% of the time
//      return RtypeInstruction(rd, rs1, rs2, MAC, KV)
//    }

    val mods = Array(VV, XV, SV, XX, SX, SS)
    val opcodes = Array(ADD, SUB, MUL, DIV, MAX, MIN)
    val mod = mods(rand.nextInt(mods.length))
    val op = opcodes(rand.nextInt(opcodes.length))
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  /**
   * Generates a random Rtype-instruction, possibly with an immediate
   * @param hasImm Whether to generate the instruction with an immediate (true) or not (false)
   * @return
   */
  def genRtype(hasImm: Boolean): RtypeInstruction = {
    import RtypeMod._
    if(!hasImm) {
      return genRtype()
    }
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)

    //Generate random immediate
    val imm = rand.nextDouble()*math.pow(2,3)*{if(rand.nextBoolean()) 1 else -1}
    //Convert to Qs3.7 number
    val immfixed = math.round(imm*math.pow(2,7))
    //Extract fractional and integer part of that fixed-point value
    val immfrac = (immfixed & 0x7f).toInt //Bits 6:0
    val immh = ((immfixed & 0x780) >> 7).toInt //Bits 10:7
    print(s"Generated instruction with immediate ${immfixed*math.pow(2,-7)}\n")

    val mods = Array(SV, SX, SS)
    val opcodes = Array(ADD, SUB, MUL, DIV, MAX, MIN)
    val mod = mods(rand.nextInt(mods.length))
    val op = opcodes(rand.nextInt(opcodes.length))
    RtypeInstruction(rd, rs2, immh, immfrac, op, mod)
    //101 0001110
  }

  /**
   * Generates an Rtype instruction with a specified opcode and modifier and random rd, rs1 and rs2 fields
   * @param op The Opcode to use
   * @param mod The modifier to use
   * @return The generated instruction
   */
  def genRtype(op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  /**
   * Takes a number of Rtype instructions and wraps them with an istart,estart,{instrs},eend,iend block
   * @param instrs The instructions to wrap
   * @param length The length of the instruction (single, Ndof or Nelem operations)
   */
  def wrapInstructions(instrs: Array[RtypeInstruction], length: OtypeLen.Type): Array[Bundle with Instruction] = {
    val istart = OtypeInstruction(se = OtypeSE.START, mod = OtypeMod.PACKET, length)
    val estart = OtypeInstruction(OtypeSE.START, mod = OtypeMod.EXEC)
    val eend = OtypeInstruction(OtypeSE.END, mod = OtypeMod.EXEC)
    val iend = OtypeInstruction(OtypeSE.END, mod = OtypeMod.PACKET)

    val a1 = Array(istart, estart).asInstanceOf[Array[Bundle with Instruction]]
    val a2 = Array(eend, iend).asInstanceOf[Array[Bundle with Instruction]]
    val ops = Array.concat(a1, instrs.asInstanceOf[Array[Bundle with Instruction]], a2)

    ops
  }

  def wrapInstructions(instrs: Array[RtypeInstruction]): Array[Bundle with Instruction] = {
    wrapInstructions(instrs, OtypeLen.SINGLE)
  }

  /**
   * Calculates the result of an ordinary arithmetic instruction
   *
   * @param instr The instruction being calculated
   * @param A The first value / numerator
   * @param b The second value / denominator
   * @return The resultint value
   */
  def calculateRes(instr: RtypeInstruction, a: SInt, b: SInt): SInt = {
    val ol = instr.op.litValue

    //If instruction is immediate, replace a-value with immediate value
    val A = if(instr.immflag.litToBoolean) {
      getImmediate(instr)
    } else {
      a
    }
    if (ol == ADD.litValue) {
      fixedAdd(A, b)
    } else if (ol == SUB.litValue()) {
      fixedSub(A, b)
    } else if (ol == MUL.litValue) {
      fixedMul(A, b)
    } else if (ol == DIV.litValue) {
      fixedDiv(A, b)
    } else if (ol == MIN.litValue) {
      fixedMin(A, b)
    } else if (ol == MAX.litValue) {
      fixedMax(A, b)
    } else if (ol == ABS.litValue) {
      fixedAbs(A)
    } else {
        throw new IllegalArgumentException("Unknown opcode")
    }
  }

  /**
   * Calculates the result of an arithmetic instruction performed elementwise on all elements in vectors a and b
   * @param instr The instruction being calculated
   * @param a Vector of first values / numerands
   * @param b Vector of second values / denominators
   * @return Vector of results
   */
  def calculateRes(instr: RtypeInstruction, a: Array[SInt], b: Array[SInt]): Array[SInt] = {
    require(a.length == b.length, "Array lengths must be equal")
    val res = Array.ofDim[SInt](a.length)

    val ol = instr.op.litValue
    if(Seq(ADD.litValue, SUB.litValue, MUL.litValue, DIV.litValue).contains(ol)) {
      for(i <- 0 until a.length) {
        res(i) = calculateRes(instr, a(i), b(i))
      }
    } else if (ol == MAC.litValue && instr.mod.litValue == RtypeMod.KV.litValue) {
      //Calculate matrix-vector product and return
    } else {
      throw new IllegalArgumentException("Uknown opcode")
    }
    res
  }



  /**
   * Calculates the outcome of a branch instruction
   * @param a The first operand
   * @param b The second operand
   * @param comp The comparison to perform
   * @return The result of a COMP b
   */
  def branchOutcome(a: SInt, b: SInt, comp: BranchComp.Type): Boolean = {
    import BranchComp._
    if(comp.litValue == EQUAL.litValue) {
      fixed2double(a) == fixed2double(b)
    } else if(comp.litValue == NEQ.litValue) {
      fixed2double(a) != fixed2double(b)
    } else if (comp.litValue() == GEQ.litValue()) {
      fixed2double(a) >= fixed2double(b)
    } else if (comp.litValue() == LT.litValue()) {
      fixed2double(a) < fixed2double(b)
    } else {
      throw new IllegalArgumentException("Unable to perform comparison")
    }
  }

  /**
   * Returns the [[RegisterFileType]] of the destination of the given instruction
   * @param instr The instruction to be parsed
   * @return The register file type of the register file where the result will be stored
   */
  def getResultRegisterType(instr: RtypeInstruction): RegisterFileType.Type = {
    import RtypeMod._
    import RegisterFileType._
    import Opcode._
    val mod = instr.mod.litValue
    if(mod == SS.litValue ||
      (mod == SV.litValue && instr.op.litValue == MAC.litValue) ||
      (mod == XX.litValue && instr.op.litValue == RED.litValue) ||
      (mod == VV.litValue && instr.op.litValue == MAC.litValue)) {
        SREG
    } else if (Seq(SX.litValue, XX.litValue).contains(mod) ||
      mod == VV.litValue && instr.op.litValue == RED.litValue) {
        XREG
    } else if (Seq(XV.litValue, SV.litValue, KV.litValue, VV.litValue).contains(mod)) {
      VREG
    } else {
      throw new IllegalArgumentException("Could not calculate result register type")
    }
  }
//
//  /**
//   * Calculates the result of an instruction with R-type modifier KV
//   * @param instr The instruction
//   * @param results Result buffer
//   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
//   */
//  def calculateKVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
//                        vReg: Array[Array[SInt]]): Unit = {
//    val KE = KEWrapper.getKEMatrix()
//    val rs1 = instr.rs1.litValue.toInt
//    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
//    //Zero out results
//    for(i <- 0 until VREG_DEPTH) {
//      results(i) = 0.S(FIXED_WIDTH.W)
//    }
//    for(i <- 0 until KE_SIZE) {
//      for(j <- 0 until KE_SIZE) {
//        val a = double2fixed(KE(i)(j)).S
//        val b = vReg(rs1*VREG_SLOT_WIDTH + rdOffset)(j)
//        results(i) = fixedAdd(results(i), fixedMul(a, b))
//      }
//    }
//  }

  /**
   * Calculates the result of an instruction with R-type modifier SS
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSSresult(instr: RtypeInstruction, results: Array[SInt],
                        sReg: Array[SInt]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = sReg(rs2)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateSXresult(instr: RtypeInstruction, results: Array[SInt],
                        sReg: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = xReg(rs2)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XX
   * @param instr The instruction
   * @param results Result buffer
   */
  def calculateXXresult(instr: RtypeInstruction, results: Array[SInt],
                        xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val imm = getImmediate(instr)
    for (i <- 0 until NUM_PROCELEM) {
      val a = if(instr.immflag.litToBoolean) imm else xReg(rs1)(i)
      val b = xReg(rs2)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier SV. Does not calculate summations (mac.sv).
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateSVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        sReg: Array[SInt], vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)
    for(i <- 0 until VREG_DEPTH) {
      val a = if(instr.immflag.litToBoolean) imm else sReg(rs1)
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }
  /**
   * Calculates the result of an instruction with R-type modifier VV. Does not calculate dot products (mac.vv)
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateVVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)

    if(instr.op.litValue == RED.litValue) { //RED.VV instructions require special care
      //Multiply all VREG_DEPTH values in each register slot, and them sum them together. Place into result register
      for(s <- 0 until VREG_SLOT_WIDTH) {
        results(s) = 0.S(FIXED_WIDTH.W)
        for(e <- 0 until VREG_DEPTH) {
          val a = vReg(rs1*VREG_SLOT_WIDTH+s)(e)
          val b = vReg(rs2*VREG_SLOT_WIDTH+s)(e)
          results(s) = fixedAdd(results(s), fixedMul(a,b))
        }
      }
    } else {
      for(i <- 0 until VREG_DEPTH) {
        val a = if(instr.immflag.litToBoolean) imm else vReg(rs1*VREG_SLOT_WIDTH+rdOffset)(i)
        val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
        results(i) = calculateRes(instr, a, b)
      }
    }
  }

  /**
   * Calculates the result of an instruction with R-type modifier XV
   * @param instr The instruction
   * @param results Result buffer
   * @param rd Current rd-value from DUT. Used to select correct vector from vector slot
   */
  def calculateXVresult(instr: RtypeInstruction, results: Array[SInt], rd: UInt,
                        xReg: Array[Array[SInt]], vReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    val rdOffset = rd.litValue.toInt % VREG_SLOT_WIDTH
    val imm = getImmediate(instr)
    val a = if(instr.immflag.litToBoolean) imm else xReg(rs1)(rdOffset)
    for(i <- 0 until VREG_DEPTH) {
      val b = vReg(rs2*VREG_SLOT_WIDTH+rdOffset)(i)
      results(i) = calculateRes(instr, a, b)
    }
  }

  /**
   * Calculates the result of a red.xx instruction
   * @param instr The instruction
   * @param results The results registers
   */
  def calculateRedXXresult(instr: RtypeInstruction, results: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rs1 = instr.rs1.litValue.toInt
    val rs2 = instr.rs2.litValue.toInt
    for(i <- results.indices) {
      results(i) = 0.S(FIXED_WIDTH.W)
    }
    for(i <- 0 until XREG_DEPTH) {
      val a = xReg(rs1)(i)
      val b = xReg(rs2)(i)
      results(0) = fixedAdd(results(0), fixedMul(a,b))
    }
  }


  /**
   * Updates the simulation vector register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   * @param rdDUT Destination register, as peeked from the DUT
   */
  def updateVREG(instr: RtypeInstruction, results: Array[SInt], rdDUT: UInt, vReg: Array[Array[SInt]]): Unit = {
    val rd = instr.rd.litValue.toInt
    //    val rdOffset = dut.io.wb.rd.reg.peek.litValue.toInt % VREG_SLOT_WIDTH
    val rdOffset = rdDUT.litValue.toInt % VREG_SLOT_WIDTH
    for (j <- 0 until VREG_DEPTH) {
      vReg(rd * VREG_SLOT_WIDTH + rdOffset)(j) = results(j)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateXREG(instr: RtypeInstruction, results: Array[SInt], xReg: Array[Array[SInt]]): Unit = {
    val rd = instr.rd.litValue.toInt
    for(i <- 0 until NUM_PROCELEM) {
      xReg(rd)(i) = results(i)
    }
  }

  /**
   * Updates the simulation register file with the values calculated in the instruction
   * @param instr The instruction that spawned these values
   * @param results The results buffer, holding the output of the instruction
   */
  def updateSREG(instr: RtypeInstruction, results: Array[SInt], sReg: Array[SInt]): Unit = {
    val rd = instr.rd.litValue.toInt
    if(rd != 0) { sReg(rd) = results(0) }
  }

  /**
   * Initializes a memory file
   * @param memfile Relative path to the memory file to initialize. Existing contents are overwritten, a new file is created if none exists
   * @param instrs The instructions to write into that file
   */
  def writeMemInitFile(memfile: String, instrs: Array[Bundle with Instruction]): Unit = {
    Assembler.writeMemInitFile(memfile, instrs.map(_.litValue.toLong))
  }

  /**
   * Generates and prints the random seed to be used for this tester.
   * A specific seed may also be passed as a parameter in the option
   * @param name The name of the test
   * @param seed a specific seed to be used instead of a randomly generated seed.
   */
  def seed(name: String, seed: Option[Long] = None): Unit = {
    val x = seed match {
      case Some(x) => x
      case None => scala.util.Random.nextLong()
    }
    scala.util.Random.setSeed(x)
    print(s"$name. Using seed $x\n")
  }

  /**
   * Wraps a load instruction with pstart, estart, eend and pend instructions to make a valid instruction packet
   * @param ldInstrs The load instructions to wrap
   * @param stInstrs An Option holding the store instructions to wrap. If None is given, no store instructions are wrapped
   * @return An array of instructions representing a full instruction packet
   */
  def wrapLoadStoreInstructions(ldInstrs: Array[StypeInstruction], stInstrs: Option[Array[StypeInstruction]] = None, len: OtypeLen.Type = OtypeLen.SINGLE): Array[Bundle with Instruction] = {
    val pstart = Array(OtypeInstruction(se=OtypeSE.START, mod = OtypeMod.PACKET, len)).asInstanceOf[Array[Bundle with Instruction]]
    val estart = OtypeInstruction(se=OtypeSE.START, mod=OtypeMod.EXEC).asInstanceOf[Bundle with Instruction]
    val eend = OtypeInstruction(se=OtypeSE.END, mod=OtypeMod.EXEC).asInstanceOf[Bundle with Instruction]
    val pend = OtypeInstruction(se=OtypeSE.END, mod=OtypeMod.PACKET).asInstanceOf[Bundle with Instruction]

    if(stInstrs.isEmpty) {
      Array.concat(pstart, ldInstrs.asInstanceOf[Array[Bundle with Instruction]], Array(estart, eend, pend))
    } else {
      Array.concat(pstart, ldInstrs.asInstanceOf[Array[Bundle with Instruction]], Array(estart, eend), stInstrs.get.asInstanceOf[Array[Bundle with Instruction]], Array(pend))
    }
  }
}

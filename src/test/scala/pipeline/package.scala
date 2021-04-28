import chisel3._
import chiseltest._
import utils.Config
import utils.Config._
import utils.Fixed._
import vector.{KEWrapper, Opcode}
import vector.Opcode._

package object pipeline {
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

    if(rand.nextInt(5) == 4) { //Generate MVP instruction and exit 20% of the time
      return RtypeInstruction(rd, rs1, rs2, MAC, KV)
    }

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
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)

    //Generate random immediate
    val imm = rand.nextDouble()*math.pow(2,3)*{if(rand.nextBoolean()) 1 else -1}
    //Convert to Qs3.7 number
    val immfixed = math.round(imm*math.pow(2,7))
    //Extract fractional and integer part of that fixed-point value
    val immfrac = (immfixed & 0x7f).toInt //Bits 6:0
    val immh = ((immfixed & 0x780) >> 7).toInt //Bits 10:7
    print(s"Generated instruction with immediate ${immfixed*math.pow(2,-7)}\n")

    val mods = Array(VV, XV, SV, XX, SX, SS)
    val opcodes = Array(ADD, SUB, MUL, DIV, MAX, MIN)
    val mod = mods(rand.nextInt(mods.length))
    val op = opcodes(rand.nextInt(opcodes.length))
    RtypeInstruction(rd, rs1, immh, immfrac, op, mod)
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
   * @param len The length of the instruction (single, Ndof or Nelem operations)
   */
  def wrapInstructions(instrs: Array[RtypeInstruction], len: Int): Array[Bundle with Instruction] = {
    var length = OtypeLen.SINGLE
    if(len == NDOF) {
      length = OtypeLen.NDOF
    } else if (len == NELEM) {
      length = OtypeLen.NELEM
    } else if (len != 1) {
      throw new IllegalArgumentException("length must be 1, NELEM or NDOF")
    }

    val istart = OtypeInstruction(se = OtypeSE.START, iev = OtypeIEV.INSTR, length)
    val estart = OtypeInstruction(OtypeSE.START, iev = OtypeIEV.EXEC)
    val eend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.EXEC)
    val iend = OtypeInstruction(OtypeSE.END, iev = OtypeIEV.INSTR)

    val a1 = Array(istart, estart).asInstanceOf[Array[Bundle with Instruction]]
    val a2 = Array(eend, iend).asInstanceOf[Array[Bundle with Instruction]]
    val ops = Array.concat(a1, instrs.asInstanceOf[Array[Bundle with Instruction]], a2)

    ops
  }

  def wrapInstructions(instrs: Array[RtypeInstruction]): Array[Bundle with Instruction] = {
    wrapInstructions(instrs, 1)
  }

  /**
   * Calculates the result of an ordinary arithmetic instruction
   *
   * @param instr The instruction being calculated
   * @param a The first value / numerator
   * @param b The second value / denominator
   * @return The resultint value
   */
  def calculateRes(instr: RtypeInstruction, a: SInt, b: SInt): SInt = {
    val ol = instr.op.litValue

    //If instruction is immediate, replace b-value with immediate value
    val B = if(instr.immflag.litToBoolean) {
      getImmediate(instr)
    } else {
      b
    }
    if (ol == ADD.litValue) {
      fixedAdd(a, B)
    } else if (ol == SUB.litValue()) {
      fixedSub(a, B)
    } else if (ol == MUL.litValue) {
      fixedMul(a, B)
    } else if (ol == DIV.litValue) {
      fixedDiv(a, B)
    } else if (ol == MIN.litValue) {
      fixedMin(a, B)
    } else if (ol == MAX.litValue) {
      fixedMax(a, B)
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
   * Computes and prints the random seed to be used for this tester.
   * @param name The name of the test
   */
  def seed(name: String): Unit = {
    val seed = scala.util.Random.nextLong()
    scala.util.Random.setSeed(seed)
    print(s"$name. Using seed $seed\n")
  }

  /**
   * Applies a configuration which requires less resources to elaborate, speeding up execution time
   */
  def genericConfig(): Unit = {
    SIMULATION = true
    NUM_VREG = 16
    VREG_DEPTH = 12
    VREG_SLOT_WIDTH = 4
    ELEMS_PER_VSLOT = VREG_DEPTH*VREG_SLOT_WIDTH
    NELEM=50 //These values don't follow the requirement that NDOF = (nx+1)*(ny+1)*(nz+1)*3
    NDOF=100
    KE_SIZE = 12
    NUM_PROCELEM = 4
    SUBVECTORS_PER_VREG  = VREG_DEPTH/NUM_PROCELEM
    FIXED_WIDTH = 24
    INT_WIDTH = 8
    FRAC_WIDTH = 15
    Config.checkRequirements()
  }
}

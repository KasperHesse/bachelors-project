import chisel3._
import chiseltest._
import utils.Config
import utils.Config._
import utils.Fixed.{FIXED_WIDTH, FRAC_WIDTH, INT_WIDTH, fixedAdd, fixedDiv, fixedMul, fixedSub}
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
    val opcodes = Array(ADD, SUB, MUL, DIV)
    val mod = mods(rand.nextInt(mods.length))
    val op = opcodes(rand.nextInt(opcodes.length))
    RtypeInstruction(rd, rs1, rs2, op, mod)
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
   * @param instr The instruction being calculated
   * @param a The first value / numerator
   * @param b The second value / denominator
   * @return The resultint value
   */
  def calculateRes(instr: RtypeInstruction, a: SInt, b: SInt): SInt = {
    val ol = instr.op.litValue
//    var res = 0.S
    //For some reason, we need to use if/else here, instead of ordinary matching
    if (ol == ADD.litValue) {
      fixedAdd(a, b)
    } else if (ol == SUB.litValue()) {
      fixedSub(a, b)
    } else if (ol == MUL.litValue) {
      fixedMul(a, b)
    } else if (ol == DIV.litValue) {
      fixedDiv(a, b)
    } else {
      throw new IllegalArgumentException("Unknown opcode")
    }
    //TODO implement support for MAC operations (esp MAC.KV operations)
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

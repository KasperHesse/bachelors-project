package utils

import chisel3._
import pipeline.InstructionFMT._
import pipeline.OtypeInstruction
import pipeline._
import utils.Config._
import vector.Opcode

object Assembler {

  def main(args: Array[String]): Unit = {
    val p = "istart\n" +
      "estart\n" +
      "add.vv vs0 vs1 vs2\n" +
      "sub.xx x1, x2, x3\n" +
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
   * Sets the bits given by [[mask]] in value [[v]], starting at index [[lowBit]]
   * @param v
   * @param lowBit
   * @param mask
   * @return
   */
  def setBits(v: Int, lowBit: Int, mask: Int): Int = {
    v | (mask << lowBit)
  }

  /**
   * Parses a string representing a vector register identifier, returning the value of the register if valid
   * @param v The string represeting the vector register
   * @return The value of register
   */
  def vReg(v: String): Int = {
    require(v.startsWith("vs"), "Vector slot indices must start with 'v'")
    val i = v.substring(2).toInt
    require(i >= 0 && i < NUM_VREG_SLOTS, s"Vector slot cannot be greater than $NUM_VREG_SLOTS")
    i
  }

  def sReg(v: String): Int = {
    require(v.startsWith("s"), "Scalar registers must be prefixed with 's'")
    val i = v.substring(1).toInt
    require(i >= 0 && i < NUM_SCALAR_REGISTERS, s"Scalar register indices must be between 0 and $NUM_SCALAR_REGISTERS")
    i
  }

  def xReg(v: String): Int = {
    require(v.startsWith("x"), s"X-registers must be prefixed with 'x', got ${v.substring(0,1)}")
    val i = v.substring(1).toInt
    require(i >= 0 && i < NUM_X_REG, s"X-register indices must be between 0 and $NUM_X_REG")
    i
  }

  /**
   * Parses a field containing an Rtype-opcode, returning the correct Opcode
   * @param v
   * @return
   */
  def opcode(v: String): Opcode.Type = {
    val op = v.split("\\.")(0)
    op match {
      case "add" => Opcode.ADD
      case "sub" => Opcode.SUB
      case "mul" => Opcode.MUL
      case "div" => Opcode.DIV
      case "mvp" => Opcode.MAC
      case _ => throw new IllegalArgumentException("Opcode not recognized")
    }
  }

  /**
   * Parses the field containing the Rtype-modifier, returning the correct RtypeMod
   * @param v The instruction of the type "OP.MOD"
   * @return The enum representing the correct modifier
   */
  def rMod(v: String): RtypeMod.Type = {
    val s = v.split("\\.")

    val op = s(0)
    //MVP instructions don't have an explicit modifier
    if(op.equals("mvp") && s.length == 1) {
      return RtypeMod.MVP
    }
    val mod = v.split("\\.")(1)
    mod match {
      case "xx" => RtypeMod.XX
      case "xv" => RtypeMod.XV
      case "ss" => RtypeMod.SS
      case "sv" => RtypeMod.SV
      case "vv" => RtypeMod.VV
      case _ => throw new IllegalArgumentException("R-type modifier not recognized")
    }
  }

  /**
   * Parses an Rtype-instruction, returning an integer containing the bit pattern representing that instruction
   * @param tokens The tokens representing the currently parsed line
   * @return An integer representing that instruction
   */
  def parseRtype(tokens: Array[String]): Int = {
    val op = opcode(tokens(0))
    val mod = rMod(tokens(0))

    val rd = mod match {
      case RtypeMod.VV | RtypeMod.XV | RtypeMod.SV | RtypeMod.MVP => vReg(tokens(1))
      case RtypeMod.SS => sReg(tokens(1))
      case RtypeMod.XX => xReg(tokens(1))
      case _ => throw new IllegalArgumentException("Unrecognized modifier for setting rd field")
    }

    val rs1 = mod match {
      case RtypeMod.VV | RtypeMod.MVP => vReg(tokens(2))
      case RtypeMod.XV |RtypeMod.XX => xReg(tokens(2))
      case RtypeMod.SS | RtypeMod.SV => sReg(tokens(2))
      case _ => throw new IllegalArgumentException("Unrecognized modifier for setting rs1 field")
    }

    val rs2 = mod match {
      case RtypeMod.VV | RtypeMod.SV | RtypeMod.XV => vReg(tokens(3))
      case RtypeMod.SS => sReg(tokens(3))
      case RtypeMod.XX => xReg(tokens(3))
      case RtypeMod.MVP => 0 //MVP instructions don't use rs2 to anything
      case _ => throw new IllegalArgumentException("Unrecognized modifier for setting rs2 field")
    }

    RtypeInstruction(rd, rs1, rs2, op, mod).litValue.toInt
  }

  /**
   * Assembles a program for the topological optimizer. Currently has limited error checking
   * @param program A string holding the program to be assembled
   * @return An array containing the instructions
   */
  def assemble(program: String): Array[Int] = {
    val prog = program.toLowerCase

    val lines = prog.trim.split("\n")

    val code = scala.collection.mutable.ListBuffer.empty[Int]
    for(line <- lines) {
      val tokens = line.split(",? +") //0 or 1 commas followed by any number of spaces
      val instr = tokens(0) match {
        case x if x.startsWith("#") => //Comment
        case "istart" => OtypeInstruction(OtypeSE.START, OtypeIEV.INSTR).toInt()
        case "iend" => OtypeInstruction(OtypeSE.END, OtypeIEV.INSTR).toInt
        case "estart" => OtypeInstruction(OtypeSE.START, OtypeIEV.ELEM).toInt
        case "eend" => OtypeInstruction(OtypeSE.END, OtypeIEV.ELEM).toInt
          //If it starts with "add.", construct an RtypeInstruction with the given parameters
        case x if x.startsWith("add.") => parseRtype(tokens)
        case x if x.startsWith("sub.") => parseRtype(tokens)
        case x if x.startsWith("mul.") => parseRtype(tokens)
        case x if x.startsWith("div.") => parseRtype(tokens)
        case x if x.startsWith("mvp") => parseRtype(tokens)
        case _ => throw new IllegalArgumentException("Unable to decode instruction")
      }

      instr match {
        case (a: Int) => code += a //Add instruction to list of instructions
        case _ => throw new IllegalArgumentException("Unable to parse instruction to a valid integer")
      }
    }
    code.toArray[Int]
    //Return instructions in reversed order
  }
}
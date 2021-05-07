package utils

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import Config._
import pipeline.{BtypeInstruction, OtypeInstruction, OtypeLen, RtypeInstruction}
import vector.Opcode._
import pipeline.RtypeMod._
import pipeline.OtypePE._
import pipeline.OtypeSE._
import pipeline.OtypeLen._
import pipeline.BranchComp._
import Fixed._

class AssemblerSpec extends FlatSpec with Matchers {
  behavior of "Assembler"

  val rand = scala.util.Random
  val opStrings = Array("add", "sub", "mul", "div", "max", "min")
  val opValues = Array(ADD, SUB, MUL, DIV, MAX, MIN)
  val modStrings = Array("vv", "xv", "sv", "xx", "sx", "ss")
  val modValues = Array(VV, XV, SV, XX, SX, SS)
  val prefixValues = Array(
    Array("v", "v", "v"), //VV
    Array("v", "x", "v"), //XV
    Array("v", "s", "v"), //SV
    Array("x", "x", "x"), //XX
    Array("x", "s", "x"), //SX
    Array("s", "s", "s")  //SS
  )
  val registerMaxValues = Array(
    Array(NUM_VREG_SLOTS, NUM_VREG_SLOTS, NUM_VREG_SLOTS), //VV
    Array(NUM_VREG_SLOTS, NUM_XREG, NUM_VREG_SLOTS), //XV
    Array(NUM_VREG_SLOTS, NUM_SREG, NUM_VREG_SLOTS), //SV
    Array(NUM_XREG, NUM_XREG, NUM_XREG), //XX
    Array(NUM_XREG, NUM_SREG, NUM_XREG), //SX
    Array(NUM_SREG, NUM_SREG, NUM_SREG) //SS
  )

  /**
   * Selects a random element from an array
   * @param arr The array to select an element from
   * @tparam T The type of the array
   * @return A random element from [[arr]]
   */
  def randomElement[T <: Any](arr: Array[T]): T = {
    arr(rand.nextInt(arr.length))
  }

  it should "assemble R-type instructions without immediates" in {
    /**
     * Performs the actual testing in here
     * @param index Index into the global arrays to use. Valid values are 0; modStrings.length
     */
    def test(index: Int): Unit = {
      val prefix = prefixValues(index) //prefix to use when generating instructions
      val regMax = registerMaxValues(index) //maximum register values to use when generating instructions

      //Selecting random opcode string and opcode
      val op = rand.nextInt(opStrings.length)
      val opString = opStrings(op)
      val opValue = opValues(op)

      //Random rtype modifier string and mod
      val modString = modStrings(index)
      val modValue = modValues(index)
      //Random rd, rs1 and rs2 values
      val rd = rand.nextInt(regMax(0))
      val rs1 = rand.nextInt(regMax(1))
      val rs2 = rand.nextInt(regMax(2))

      val line = f"$opString.$modString ${prefix(0)}$rd, ${prefix(1)}$rs1, ${prefix(2)}$rs2"

      val parsed = Assembler.parseRtype(Assembler.split(line))
      val instr = RtypeInstruction(rd, rs1, rs2, opValue, modValue).litValue.toInt
      assert(parsed == instr)
    }
    test(0)
    test(1)
    test(2)
    test(3)
    test(4)
    test(5)
  }

  it should "assemble R-type instructions with immediates" in {
    val modStrings = Array("iv", "ix", "is")
    val modValues = Array(SV, SX, SS)

    val prefixValues = Array("v", "x", "s")
    val registerMaxValues = Array(NUM_VREG_SLOTS, NUM_XREG, NUM_SREG)

    def testFun(index: Int): Unit = {
      val modString = modStrings(index)
      val modValue = modValues(index)
      val prefix = prefixValues(index)
      val regMax = registerMaxValues(index)

      val op = rand.nextInt(opStrings.length)
      val opString = opStrings(op)
      val opValue = opValues(op)

      val rs1 = rand.nextInt(regMax)
      val rd = rand.nextInt(regMax)

      val imm = genImmediate()
      val immparts = fixedImm2parts(imm2fixed(imm))

      val line = s"$opString.$modString $prefix$rd, $prefix$rs1, $imm"
      val parsed = Assembler.parseRtype(Assembler.split(line))

      val instr = RtypeInstruction(rd, rs1, immparts(0), immparts(1), opValue, modValue).litValue.toInt
      assert(parsed == instr)
    }

    testFun(0)
    testFun(1)
    testFun(2)
  }

  it should "throw an error if no immediate value is given" in {
    try {
      Assembler.parseRtype(Assembler.split("add.iv v0, v0, v0"))
      assert(false)
    } catch {
      case e: NumberFormatException => assert(true)
    }
  }

  it should "assemble O-type instructions" in {
    val lines = Array("pstart single",
    "pstart ndof",
    "pstart nelem",
    "estart",
    "eend",
    "pend")
    val instrs = Array(
      (START, PACKET, SINGLE),
      (START, PACKET, OtypeLen.NDOF),
      (START, PACKET, OtypeLen.NELEM),
      (START, EXEC, SINGLE),
      (END, EXEC, SINGLE),
      (END, PACKET, SINGLE)
    )
    for(i <- lines.indices) {
      val instr = OtypeInstruction(instrs(i)._1, instrs(i)._2, instrs(i)._3)
      val parsed = Assembler.parseOtype(Assembler.split(lines(i)))
      assert(parsed == instr.litValue.toInt)
    }
  }

  it should "assemble B-type instructions" in {
    val compValues = Array(EQUAL, NEQ, LT, GEQ)
    val compStrings = Array("beq", "bne", "blt", "bge")
    //Branch type, registers and offset.
    //Offset should be 2^15 and then multiplied by 4
    def testFun(compIndex: Int): Unit = {
      val offset = rand.nextInt(math.pow(2,15).toInt) * 4 * (if(rand.nextBoolean()) 1 else -1)
      val rs1 = rand.nextInt(NUM_SREG)
      val rs2 = rand.nextInt(NUM_SREG)
      val compValue = compValues(compIndex)
      val compString = compStrings(compIndex)

      val line = s"$compString s$rs1, s$rs2, $offset"
      val instr = BtypeInstruction(compValue, rs1, rs2, offset)
      val parsed = Assembler.parseBtype(Assembler.split(line))
      assert(parsed == instr.litValue().toInt)
    }

    for(i <- 0 until 10) {
      testFun(0)
      testFun(1)
      testFun(2)
      testFun(3)
    }
  }

  it should "throw an error if branch offset is not a multiple of 4" in {
    try {
      Assembler.parseBtype(Assembler.split("beq s0, s0, 1"))
      assert(false, "Did not correctly throw an exception")
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
  }

  it should "assemble S-type instructions" in {
//
//    val program = "add.vv v0, v1, v2\n" +
//      "sub.xv v1, x3, v2\n" +
//      "add.lv v0, v2, 3\n" +
//      "\tbeq s4, s0, -4\n\n\n"
//
//    val instrs = Assembler.assemble(program)
//    val INSTRS = Array(
//      RtypeInstruction(0, 1, 2, ADD, VV),
//      RtypeInstruction(1, 3, 2, SUB, XV),
//      RtypeInstruction(0, 2, 3, 0, ADD, SV),
//      BtypeInstruction(EQUAL, 0, 0, -12)
//    )
//
//    for(i <- instrs.indices) {
//      println(instrs(i) + " " + INSTRS(i).litValue.toInt)
//    }

  }

  it should "disallow R-type instructions outside of execution packets" in {
    val programs = Array(
      "add.vv v0, v1, v2",
      "pstart single\n" +
        "add.vv v0, v1, v2",
      "pstart single\n" +
        "estart\n" +
        "eend\n" +
        "add.vv v0, v1, v2")

    for(program <- programs) {
      try {
        Assembler.assemble(program)
        assert(false, "Wrongly assembled R-type instrutions outside of estart")
      } catch {
        case e: IllegalArgumentException => {
          if(e.getMessage().contains("R-type instructions only allowed between estart and eend")) {
            assert(true)
          } else assert(false, "Wrongly assembled R-type instructions outside of estart")
        }
      }
    }
  }

  it should "disallow load instructions outside of the load section" in {
    val programs = Array(
      "ld.vec v0, X",
      "pstart single\n" +
        "estart\n" +
        "ld.vec v0, X",
      "pstart single\n" +
        "estart\n" +
        "eend\n" +
        "ld.vec v0, X"
    )
    for(program <- programs) {
      try {
        Assembler.assemble(program)
        assert(false, "Wrongly assembled load instrutions outside of load section")
      } catch {
        case e: IllegalArgumentException => {
          if(e.getMessage().contains("Load instructions only allowed between pstart and estart")) {
            assert(true)
          } else assert(false, "Wrongly assembled load instrutions outside of load section")
        }
      }
    }
  }

  it should "disallow store instructions outside of the store section" in {
    val programs = Array(
      "st.vec v0, X",
      "pstart ndof\n" +
        "st.vec v0, X",
      "pstart ndof\n" +
        "estart\n" +
        "st.vec v0, X",
    )
    for(program <- programs) {
      try {
        Assembler.assemble(program)
        assert(false, "Wrongly assembled store instrutions outside of store section")
      } catch {
        case e: IllegalArgumentException => {
          if(e.getMessage().contains("Store instructions only allowed between eend and pend")) {
            assert(true)
          } else assert(false, "Wrongly assembled store instrutions outside of store section")
        }
      }
    }
  }

  it should "disallow nested pstart instructions" in {
    val program = "pstart single\n" +
      "pstart single"
    try {
      Assembler.assemble(program)
      assert(false, "Wrongly assembled nested pstart instrutions")
    } catch {
      case e: IllegalArgumentException => {
        if(e.getMessage().contains("Cannot have nested pstart instructions")) {
          assert(true)
        } else assert(false, "Wrongly assembled nested pstart instrutions")
      }
    }
  }

  it should "disallow nested estart instructions" in {
    val program = "pstart single\n" +
      "estart\n" +
      "estart"
    try {
      Assembler.assemble(program)
      assert(false, "Wrongly assembled nested estart instrutions")
    } catch {
      case e: IllegalArgumentException => {
        if(e.getMessage().contains("Cannot have nested estart instructions")) {
          assert(true)
        } else assert(false, "Wrongly assembled nested estart instrutions")
      }
    }
  }

  it should "disallow branches inside instruction packets" in {
    val program = "pstart single\n" +
      "beq s0, s0, L2"
    try {
      Assembler.assemble(program)
      assert(false, "Wrongly assembled branch instruction inside packet")
    } catch {
      case e: IllegalArgumentException =>  {
        if(e.getMessage.contains("Branch instructions cannot be inside an instruction packet")) {
          assert(true)
        } else assert(false, "Wrongly assembled branch instruction inside packet")
      }
    }
  }

  it should "only allow MAC instructions with KV, SV and VV suffix" in {
    try {
      Assembler.parseRtype(Assembler.split("mac.xx x0, x1, x2"))
    } catch {
      case e: IllegalArgumentException => if(e.getMessage.contains("MAC instructions can only be executed with modifiers 'kv', 'sv', and 'vv'")) assert(true) else assert(false)
    }
  }

  it should "only allow the KV suffix on MAC instructions" in {
    val parsed = Assembler.parseRtype(Assembler.split("mac.kv v0, v1"))
    val instr = RtypeInstruction(0, 1, 0, MAC, KV)
    assert(parsed == instr.litValue.toInt)
    try {
      Assembler.parseRtype(Assembler.split("add.kv v0, v1"))
    } catch {
      case e: IllegalArgumentException => if(e.getMessage.contains("KV instructions can only have the MAC opcode")) assert(true) else assert(false)
    }
  }

  it should "only allow one long MAC instruction per packet" in {
    val program = "pstart single\n" +
      "estart\n" +
      "mac.kv v0, v1\n" +
      "mac.vv s0, v1, v2"
    try {
      Assembler.assemble(program)
      assert(false, "Wrongly assembled two mac instructions")
    } catch {
      case e: IllegalArgumentException => if(e.getMessage.contains("There can only be one mac instruction in each instruction packet")) assert(true) else assert(false, "Wrongly assembled two mac instructions")
    }
  }

  it should "ignore comments" in {
    Assembler.assemble("//")
    assert(true)
  }

  it should "throw an error when register indices are too large" in {
    try {
      Assembler.vReg(s"v${NUM_VREG_SLOTS}")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
    try {
      Assembler.xReg(s"x${NUM_XREG}")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
    try {
      Assembler.sReg(s"s${NUM_SREG}")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
  }

  it should "throw an error when the wrong register prefix is given" in {
    try {
      Assembler.vReg(s"0")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
    try {
      Assembler.xReg(s"0")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
    try {
      Assembler.xReg(s"0")
      assert(false)
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
  }

  it should "throw an error if a register index is not recognized" in {
    try {
      Assembler.vReg(s"vv")
      assert(false)
    } catch {
      case e: NumberFormatException => assert(true)
    }
    try {
      Assembler.xReg(s"xx")
      assert(false)
    } catch {
      case e: NumberFormatException => assert(true)
    }
    try {
      Assembler.sReg(s"ss")
      assert(false)
    } catch {
      case e: NumberFormatException => assert(true)
    }
  }

  it should "only allow .iv, .ix and .is suffix for immediate instructions" in {
    try {
      Assembler.parseRtype(Assembler.split("add.iv v0, v1, v2"))
    } catch {
      case e: NumberFormatException => if(e.getMessage.contains("Unable to parse")) assert(true) else assert(false)
    }
  }

  it should "not allow .iv, .ix and .is suffix for non-immediate instructions" in {
    try {
      Assembler.parseRtype(Assembler.split("add.vv v0, v1, 5"))
    } catch {
      case e: IllegalArgumentException => if(e.getMessage.contains("Vector slot indices must start with")) assert(true) else assert(false)
    }
  }

  it should "only allow instruction packets of a certain size" in {
    //Probably 32
    ???
  }

  it should "perform substituion of mvp pseudoinstructions" in {
    ???
  }

  it should "perform substitution of sqrt pseudoinstruction" in {
    ???
  }
}

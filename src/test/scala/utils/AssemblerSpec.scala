package utils

import chisel3._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import Config._
import execution.{BtypeInstruction, OtypeInstruction, OtypeLen, RtypeInstruction, StypeInstruction}
import execution.Opcode._
import execution.RtypeMod._
import execution.OtypeMod._
import execution.OtypeSE._
import execution.OtypeLen._
import execution.BranchComp._
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
    for(i <- 0 until 5) {
      test(0)
      test(1)
      test(2)
      test(3)
      test(4)
      test(5)
    }
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

      val rs2 = rand.nextInt(regMax)
      val rd = rand.nextInt(regMax)

      val imm = genImmediate()
      val immparts = fixedImm2parts(imm2fixed(imm))

      val line = s"$opString.$modString $prefix$rd, $prefix$rs2, $imm"
      val parsed = Assembler.parseRtype(Assembler.split(line))

      val instr = RtypeInstruction(rd, rs2, immparts(0), immparts(1), opValue, modValue).litValue.toInt
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
    val lines = Array(
      "pstart single",
      "pstart ndof",
      "pstart nelemvec",
      "pstart nelemdof",
      "pstart nelemstep",
      "estart",
      "eend",
      "pend",
      "tstart run",
      "tstart clear",
      "tend"
    )
    val instrs = Array(
      (START, PACKET, SINGLE),
      (START, PACKET, OtypeLen.NDOF),
      (START, PACKET, OtypeLen.NELEMVEC),
      (START, PACKET, OtypeLen.NELEMDOF),
      (START, PACKET, OtypeLen.NELEMSTEP),
      (START, EXEC, SINGLE),
      (END, EXEC, SINGLE),
      (END, PACKET, SINGLE),
      (START, TIME, OtypeLen.SINGLE),
      (START, TIME, OtypeLen.NDOF),
      (END, TIME, OtypeLen.SINGLE)
    )
    for(i <- lines.indices) {
      val instr = OtypeInstruction(se=instrs(i)._1, mod=instrs(i)._2, len=instrs(i)._3)
      val parsed = Assembler.parseOtype(Assembler.split(lines(i)), Array(0))
      assert(parsed == instr.litValue.toInt)
      assert(instr.litValue.toInt == OtypeInstruction(parsed.U).litValue.toInt)
    }
  }

  it should "only take one operand when parsing abs instruction" in {
    Assembler.parseRtype(Assembler.split("abs.xx x0, x1"))
  }

  it should "throw an error if branch offset is not a multiple of 4" in {
    try {
      val map = scala.collection.mutable.Map[String, Int]()
      Assembler.parseBtype(Assembler.split("beq s0, s0, 1"), map, 0)
      assert(false, "Did not correctly throw an exception")
    } catch {
      case e: IllegalArgumentException => assert(true)
    }
  }

  it should "use the symbol table to generate branch instructions" in {
    val program = "" +
    "L1:\n" +
      "pstart single\n" + //0
      "estart\n" + //4
      "add.is s1, s1, 1\n" + //8
      "add.is s2, s0, 5\n" + //12
      "eend\n" + //16
      "pend\n" + //20
      "bne s1, s2, L2\n" + //24
      "beq s0, s0, L1\n" + //28
      "L2:\n" +
      "blt s1, s1, L1" //32
    val parsed = Assembler.assemble(program)
    val p1 = execution.wrapInstructions(Array(RtypeInstruction(1, 1, 1, 0, ADD, SS), RtypeInstruction(2, 0, 5, 0, ADD, SS)))
    val b1 = Array(BtypeInstruction(NEQ, 1, 2, 8), BtypeInstruction(EQUAL, 0, 0, -28), BtypeInstruction(LT, 1, 1, -32)).asInstanceOf[Array[Bundle with execution.Instruction]]
    val instrs = Array.concat(p1, b1)
    for(i <- instrs.indices) {
      assert(parsed(i) == instrs(i).litValue.toInt)
    }
  }

  it should "throw an error if symbols are not alone on their line" in {
    val program = "" +
    "L1: pstart single\n"
    try {
      Assembler.assemble(program)
      assert(false, "Did not catch a bad label")
    } catch {
      case e if e.getMessage.contains("Labels must be on their own line") => assert(true)
      case _: Exception => assert(false, "Did not catch a bad label")
    }
  }

  it should "throw an error on duplicate labels" in {
    val program = "" +
    "L1:\n" +
      "L1:\n" +
      "pstart single"
    try {
      Assembler.assemble(program)
      assert(false, "Did not catch duplicate label")
    } catch {
      case e: Exception => {
        if (e.getMessage.contains("Duplicate label")) assert(true) else assert(false, "Did not catch duplicate label")
      }
      case _: Exception => assert(false, "Did not catch duplicate label")
    }
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

  it should "ignore opening and closing curly braces" in {
    val program = "{\n" +
      "pstart single\n" +
      "estart\n" +
      "eend\n" +
      "pend\n" +
      "}"
    Assembler.assemble(program)
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

  it should "assemble S-type ld.vec instructions" in {
    import execution.StypeMod._
    import execution.StypeBaseAddress._
    val lines = Array(
      "ld.vec v0, x",
      "ld.vec v1, xphys",
      "ld.vec v2, xnew",
      "ld.vec v3, dc",
      "ld.vec v0, dv",
      "ld.vec v1, f",
      "ld.vec v2, u",
      "ld.vec v3, r",
      "ld.vec v0, z",
      "ld.vec v1, p",
      "ld.vec v2, q",
      "ld.vec v3, invd",
      "ld.vec v0, tmp"
    )
    val instrs = Array(
      StypeInstruction(0, VEC, X),
      StypeInstruction(1, VEC, XPHYS),
      StypeInstruction(2, VEC, XNEW),
      StypeInstruction(3, VEC, DC),
      StypeInstruction(0, VEC, DV),
      StypeInstruction(1, VEC, F),
      StypeInstruction(2, VEC, U),
      StypeInstruction(3, VEC, R),
      StypeInstruction(0, VEC, Z),
      StypeInstruction(1, VEC, P),
      StypeInstruction(2, VEC, Q),
      StypeInstruction(3, VEC, INVD),
      StypeInstruction(0, VEC, TMP)
    )

    (lines,instrs).zipped.foreach((line,instr) =>
    {
      val parsed = Assembler.parseStype(Assembler.split(line), Array(LitVals.NDOF))
      val i = instr.toUInt().litValue.toInt
      assert(parsed == i)
    })
  }

  it should "disallow ld.fdof instructions" in {
    val instr = "ld.fdof v1, u"
    try {
      Assembler.parseStype(Assembler.split(instr), Array(LitVals.NELEMDOF))
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.toLowerCase.contains("cannot perform")) assert(true) else assert(false)
    }
  }

  it should "only allow dof/fdof operations to NDOF long vectors" in {
    val prefix = "st.dof v0, "
    val suffixPass = Array("f", "u", "r", "z", "p", "q", "invd", "tmp")
    val suffixFail = Array("x", "xphys", "xnew", "dc", "dv")
    val pass = suffixPass.map(prefix + _)
    val fail = suffixFail.map(prefix + _)
    for(p <- pass) {
      val instr = Assembler.parseStype(Assembler.split(p), Array(LitVals.NELEMDOF)) //Should be fine
    }
    for(f <- fail) {
      try {
        Assembler.parseStype(Assembler.split(f), Array(LitVals.NELEMDOF))
      } catch {
        case e: IllegalArgumentException => if (!e.getMessage.toLowerCase.contains("cannot perform")) assert(false)
      }
    }
  }

  it should "only allow elem/sel/fcn/edn1/edn2 operations on NELEM long vectors" in {
    val prefix = "ld.elem x0, "
    val suffixPass = Array("x", "xphys", "xnew", "dc", "dv")
    val suffixFail = Array("f", "u", "r", "z", "p", "q", "invd", "tmp")
    val pass = suffixPass.map(prefix + _)
    val fail = suffixFail.map(prefix + _)
    for(p <- pass) {
      Assembler.parseStype(Assembler.split(p), Array(LitVals.NELEMDOF)) //Should be fine
    }
    for(f <- fail) {
      try {
        Assembler.parseStype(Assembler.split(f), Array(LitVals.NELEMDOF))
      } catch {
        case e: IllegalArgumentException => if (!e.getMessage.toLowerCase.contains("cannot perform")) assert(false)
      }
    }
  }

  it should "only allow instruction packets of a certain size" in {
    val pstart = "pstart single\n"
    val estart = "estart\n"
    val instr = "add.vv v0, v1, v2\n"
    val eend = "eend\n"
    val pend = "pend"
    val pass = s"$pstart$estart${instr * (INSTRUCTION_BUFFER_SIZE-4)}$eend$pend"
    val fail = s"$pstart$estart${instr * (INSTRUCTION_BUFFER_SIZE-3)}$eend$pend"

    Assembler.assemble(pass) //Should be good
    try {
      Assembler.assemble(fail)
      assert(false)
    } catch {
      case e: IllegalArgumentException => if (e.getMessage.contains("packet too large")) assert(true) else assert(false)
    }
  }
}

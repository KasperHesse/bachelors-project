package execution

import chisel3._
import chiseltest._

import utils.Fixed._
import chisel3.experimental.BundleLiterals._
import chisel3.util.DecoupledIO
import utils.Config
import utils.Config._
import Opcode._
import execution.ThreadState._
import execution.StypeMod._
import execution.StypeBaseAddress._
import execution.StypeLoadStore._
import memory.{IJKBundle, IJKgeneratorConsumerIO, ReadQueueBundle, genIJKmultiple}
import execution.RegisterFileType._
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThreadSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers {

  def expectVVvalues(dut: Thread, inst: RtypeInstruction): Unit = {
    val vReg = dut.vRegFile.arr
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd.litValue.toInt
    for(s <- 0 until VREG_SLOT_WIDTH) {
      for (i <- 0 until VREG_DEPTH by NUM_PROCELEM) {
        dut.io.ex.a(0).expect(vReg(s + rs1 * VREG_SLOT_WIDTH)(i))
        dut.io.ex.b(0).expect(vReg(s + rs2 * VREG_SLOT_WIDTH)(i))
        dut.io.ex.dest.reg.expect((rd*VREG_SLOT_WIDTH + s).U)
        dut.io.ex.dest.subvec.expect((i / NUM_PROCELEM).U)
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
        dut.io.ex.op.expect(inst.op)
        dut.clock.step()
      }
    }
  }

  def expectXVvalues(dut: Thread, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val subvecsPerVreg = VREG_DEPTH/NUM_PROCELEM
    val vReg = dut.vRegFile.arr
    val xReg = dut.xRegFile.arr

    for(s <- 0 until VREG_SLOT_WIDTH) {
      for(i <- 0 until subvecsPerVreg) {
        for (j <- 0 until NUM_PROCELEM) {
          dut.io.ex.a(j).expect(xReg(rs1)(s))
          dut.io.ex.b(j).expect(vReg(s+rs2*VREG_SLOT_WIDTH)(i*NUM_PROCELEM+j))
        }
        dut.clock.step()
        dut.io.ex.dest.rf.expect(RegisterFileType.VREG)
      }
    }
  }

  def expectXXvalues(dut: Thread, inst: RtypeInstruction): Unit = {
    val rs1 = inst.rs1.litValue.toInt
    val rs2 = inst.rs2.litValue.toInt
    val rd = inst.rd
    val xReg = dut.xRegFile.arr

    for(i <- 0 until NUM_PROCELEM) {
      dut.io.ex.a(i).expect(xReg(rs1)(i))
      dut.io.ex.b(i).expect(xReg(rs2)(i))
      dut.io.ex.dest.reg.expect(rd)
      dut.io.ex.dest.subvec.expect(0.U)
      dut.io.ex.dest.rf.expect(RegisterFileType.XREG)
    }
    dut.clock.step()
  }

  def expectValues(dut: Thread, instr: UInt): Unit = {
    val i = RtypeInstruction(instr)
    val mod = i.mod.litValue
    if(mod == RtypeMod.VV.litValue) {
      expectVVvalues(dut, i)
    } else if (mod == RtypeMod.XV.litValue) {
      expectXVvalues(dut, i)
    } else if (mod == RtypeMod.XX.litValue) {
      expectXXvalues(dut, i)
    } else {
      throw new IllegalArgumentException("Unknown Rtype modifier")
    }
  }

  def genRtype(op: Opcode.Type, mod: RtypeMod.Type): RtypeInstruction = {
    val rand = scala.util.Random
    val rd = rand.nextInt(NUM_VREG_SLOTS)
    val rs1 = rand.nextInt(NUM_VREG_SLOTS)
    val rs2 = rand.nextInt(NUM_VREG_SLOTS)
    RtypeInstruction(rd, rs1, rs2, op, mod)
  }

  def genInstructions(dut: Thread, mod: RtypeMod.Type): Array[Bundle with Instruction] = {
    val istart = OtypeInstruction(se = OtypeSE.START, mod = OtypeMod.PACKET)
    val estart = OtypeInstruction(OtypeSE.START, mod = OtypeMod.EXEC)
    val eend = OtypeInstruction(OtypeSE.END, mod = OtypeMod.EXEC)
    val iend = OtypeInstruction(OtypeSE.END, mod = OtypeMod.PACKET)
    val add = genRtype(ADD, mod)
    val sub = genRtype(SUB, mod)
    val mul = genRtype(MUL, mod)
    val div = genRtype(DIV, mod)

    val ops = Array(istart, estart, add, div, mul, sub, eend, iend)
    ops
  }

  /**
   * Tests the thread module for correctly decoding instruction packets that only contain execution instructions
   * @param dut The Thread DUT
   * @param mod The Rtype-modifier to be used when generating instructions.
   * @param numRes Number of results which should be observed before finishing the test
   * @note Should only be used for instruction packets of the kind (istart, estart, {instructions}, eend, iend)
   */
  def testThread(dut: Thread, mod: RtypeMod.Type, numRes: Int = 4): Unit = {
    val instrs = genInstructions(dut, mod)

    //Poke default values
    dut.io.fin.poke(false.B)
    dut.io.threadIn.state.poke(sEstart)
    dut.io.instr.poke(0.U)
    dut.io.start.poke(true.B)

    var fin: Boolean = false
    var i: Int = 0
    var resCnt: Int = 0

    while(!fin && i < 300) {
      val ip = dut.io.ip.peek().litValue.toInt
      dut.io.instr.poke(instrs(ip).toUInt())

      //Assign stateIn to speed through load and store stages
      if(dut.io.stateOutUint.peek().litValue == sLoad.litValue) {
        dut.io.threadIn.state.poke(sEend)
        dut.io.start.poke(false.B)
      } else if (dut.io.stateOutUint.peek().litValue == sExec.litValue) {
        dut.io.threadIn.state.poke(sEstart)
      }

      //Assert 'fin' once end state has been reached
      if(dut.io.stateOutUint.peek().litValue == sPend.litValue && resCnt >= numRes) {
        fin = true
      }
      dut.io.fin.poke(fin.B)

      //Expect output values. Start doing this on the first clock cycle of each instruction
      //Currently pretty ugly, but that's what we have to work with, I guess
      //We need the final check since IP is kept constant for one cycle before moving to sLoad, and we should not expect in that cycle
      if(dut.io.stateOutUint.peek().litValue == sExec.litValue && dut.io.ctrl.firstCycle.peek().litToBoolean && dut.io.ex.valid.peek().litToBoolean) {
        expectValues(dut, instrs(ip).toUInt())
        resCnt += 1
      } else {
        dut.clock.step()
      }

      i += 1
    }
    dut.clock.step(3)
    assert(resCnt == numRes)
  }

  behavior of "Thread without memory access"

  it should "test VV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("VV thread decode")
    val id = scala.util.Random.nextInt(2)
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.VV)
    }
  }

  it should "test XV instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("XV thread decode")
    val id = scala.util.Random.nextInt(2)
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.XV)
    }
  }

  it should "test XX instruction load and decode" in {
    SIMULATION = true
    Config.checkRequirements()
    seed("XX thread decode")
    val id = scala.util.Random.nextInt(2)
    test(new Thread(id)) {dut =>
      testThread(dut, RtypeMod.XX)
    }
  }

  it should "go back to idle when id=1 and fin=true" in {
    SIMULATION = true
    Config.checkRequirements()
    test(new Thread(1)) {dut =>
      val istart = OtypeInstruction(se = OtypeSE.START, mod = OtypeMod.PACKET)

      dut.io.start.poke(true.B)
      dut.io.fin.poke(true.B)
      dut.io.instr.poke(istart.toUInt())
      dut.clock.step()

      assert(dut.io.stateOutUint.peek().litValue == sWait1.litValue)
      dut.io.threadIn.state.poke(sEstart)
      dut.clock.step()

      assert(dut.io.stateOutUint.peek().litValue == sWait2.litValue)
      dut.clock.step(5)
      assert(dut.io.stateOutUint.peek().litValue == sWait2.litValue)
      dut.io.threadIn.state.poke(sEend)
      dut.clock.step()

      assert(dut.io.stateOutUint.peek().litValue == sIdle.litValue)
    }
  }

  behavior of "Thread with memory access"



  /**
   * Performs an expect on an output port implementing [[IJKgeneratorConsumerIO]] with the [[DecoupledIO]] interface.
   * @param port The output port to expect data on. Must
   * @param baseAddr The base addresss of the instruction
   * @param ijk The ijk-values of the instruction
   * @param mod The S-type modifier of the instruction
   * @param pad Padding flag for the instruction
   * @param valid Valid output bit. Defaults to true
   */
  def expectIJK(port: DecoupledIO[IJKgeneratorConsumerIO], baseAddr: StypeBaseAddress.Type, ijk: Array[Int], mod: StypeMod.Type, pad: Boolean, valid: Boolean = true): Unit = {
    port.valid.expect(valid.B)
    port.bits.baseAddr.expect(baseAddr)
    port.bits.ijk.expect((new IJKBundle).Lit(_.i -> ijk(0).U, _.j -> ijk(1).U, _.k -> ijk(2).U))
    port.bits.mod.expect(mod)
    port.bits.pad.expect(pad.B)
  }

  /**
   * Performs an expect on a read queue output implementing [[ReadQueueBundle]] with the [[DecoupledIO]] interface.
   * @param rq The read queue port to expect data on
   * @param iter The iteration number expected in the outupt
   * @param reg The destination register number
   * @param rf The destination register file type
   */
  def expectReadQueue(rq: DecoupledIO[ReadQueueBundle], iter: Int, reg: Int, rf: RegisterFileType.Type): Unit = {
    rq.valid.expect(true.B)
    rq.bits.iter.expect(iter.U)
    rq.bits.rd.reg.expect(reg.U)
    rq.bits.rd.rf.expect(rf)
  }

  it should "generate outputs for an ELEM instruction" in {
    val elem = StypeInstruction(rsrd=0, mod=ELEM, baseAddr = XPHYS, ls=LOAD)
    val IJK = genIJKmultiple(start=Some(Array(0,0,0,0)))
    val instrs = wrapLoadStoreInstructions(Array(elem))

    test(new Thread(0)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      var fin: Boolean = false
      var i: Int = 0
      dut.io.mem.vec.ready.poke(true.B)
      dut.io.mem.edof.ready.poke(true.B)
      dut.io.mem.neighbour.ready.poke(true.B)
      dut.io.mem.readQueue.ready.poke(true.B)
      dut.io.fin.poke(false.B)
      dut.io.threadIn.state.poke(sEstart)
      dut.io.start.poke(true.B)

      while(!fin & i < 50) {
        fork {
          //Poke the wanted instruction
          val ip = dut.io.ip.peek().litValue.toInt
          val instr = instrs(ip)
          dut.io.instr.poke(instr.toUInt())

          //Observe outputs
          if(dut.io.mem.neighbour.valid.peek().litToBoolean) {
            for(i <- IJK.indices) {
              val ijk = IJK(i)
              expectIJK(dut.io.mem.neighbour, elem.baseAddr, ijk, elem.mod, pad=false)
              expectReadQueue(dut.io.mem.readQueue, ijk(3), reg=0, rf=XREG)
              dut.clock.step()
            }
          } else {
            dut.clock.step()
          }
        } .fork { //Watch end of instruction
          if(dut.io.ctrl.stateUint.peek().litValue === ThreadState.sEstart.litValue.toInt) {
            fin = true
          }
          dut.clock.step()
          i += 1
        }.join()
      }
      assert(fin)
    }
  }

  it should "generate outputs for a DOF instruction" in {
    val rd = 2
    val dof = StypeInstruction(rsrd=2, mod=DOF, baseAddr=P, ls=LOAD)
    val IJK = genIJKmultiple(start=Some(Array(0,0,0,0)))
    val instrs = wrapLoadStoreInstructions(Array(dof))

    test(new Thread(0)).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      var fin: Boolean = false
      var i: Int = 0
      dut.io.mem.vec.ready.poke(true.B)
      dut.io.mem.edof.ready.poke(true.B)
      dut.io.mem.neighbour.ready.poke(true.B)
      dut.io.mem.readQueue.ready.poke(true.B)
      dut.io.fin.poke(false.B)
      dut.io.threadIn.state.poke(sEstart)
      dut.io.start.poke(true.B)
      while(!fin & i < 50) {
        fork {
          //Poke the wanted instruction
          val ip = dut.io.ip.peek().litValue.toInt
          val instr = instrs(ip)
          dut.io.instr.poke(instr.toUInt())

          //Observe outputs
          if(dut.io.mem.edof.valid.peek().litToBoolean) {
            for(i <- 0 until IJK.length*SUBVECTORS_PER_VREG) {
              val j = i/SUBVECTORS_PER_VREG
              val ijk = IJK(j)
              if(i % SUBVECTORS_PER_VREG==0) expectIJK(dut.io.mem.edof, dof.baseAddr, ijk, dof.mod, pad=false)
              expectReadQueue(dut.io.mem.readQueue, iter=0, reg=rd*VREG_SLOT_WIDTH + j, rf=VREG)
              dut.clock.step()
              if(i % SUBVECTORS_PER_VREG == 0) {
                dut.io.mem.edof.ready.poke(false.B)
              } else if (i % SUBVECTORS_PER_VREG == SUBVECTORS_PER_VREG-1) {
                dut.io.mem.edof.ready.poke(true.B)
              }
            }
          } else {
            dut.clock.step()
          }
        } .fork { //Watch end of instruction
          if(dut.io.ctrl.stateUint.peek().litValue === ThreadState.sEstart.litValue.toInt) {
            fin = true
          }
          dut.clock.step()
          i += 1
        }.join()
      }
      assert(fin)
    }
  }
}

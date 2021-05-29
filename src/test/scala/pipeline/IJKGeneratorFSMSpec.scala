package pipeline

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import pipeline.ThreadState._
import StypeLoadStore._
import StypeMod._
import InstructionFMT._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.experimental.TestOptionBuilder._
import memory.{IJKBundle, genIJK, genIJKmultiple, nextIJK}
import utils.Fixed._
import utils.Config._


class IJKGeneratorFSMSpec extends FlatSpec with ChiselScalatestTester with Matchers {
  behavior of "Memory Access FSM"

  def expectIJK(ijk: IJKBundle, i: Int, j: Int, k: Int): Unit = {
    ijk.i.expect(i.U)
    ijk.k.expect(k.U)
    ijk.j.expect(j.U)
  }

  def expectIJK(dut: IJKGeneratorFSM, i: Int, j: Int, k: Int): Unit = {
    expectIJK(dut.io.ijkOut.ijk, i, j, k)
    expectIJK(dut.io.edof.bits.ijk, i, j, k)
    expectIJK(dut.io.neighbour.bits.ijk, i, j, k)
  }

  def peekIJK(dut: IJKGeneratorFSM): Array[Int] = {
    Array(dut.io.ijkOut.ijk.i.peek.litValue.toInt, dut.io.ijkOut.ijk.j.peek.litValue.toInt, dut.io.ijkOut.ijk.k.peek.litValue.toInt)
  }

  def pokeIJK(dut: IJKGeneratorFSM, i: Int, j: Int, k: Int, iter: Int): Unit = {
    dut.io.ijkIn.ijk.i.poke(i.U)
    dut.io.ijkIn.ijk.j.poke(j.U)
    dut.io.ijkIn.ijk.k.poke(k.U)
    dut.io.ijkIn.iteration.poke(iter.U)
  }

  it should "keep all valid signals low when not in sStore or Sload" in {
    test(new IJKGeneratorFSM) { dut =>
      val stateList = Seq(sIdle, sEstart, sExec, sEend, sPend, sWait1, sWait2)
      for(state <- stateList) {
        dut.io.threadState.poke(state)
        dut.clock.step()
        dut.io.edof.valid.expect(false.B)
        dut.io.neighbour.valid.expect(false.B)
      }
    }
  }

  it should "output multiple values when mod is DOF or ELEM" in {
    test(new IJKGeneratorFSM).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val dof = StypeInstruction(rsrd = 0, mod=DOF, offset = StypeBaseAddress.X, LOAD).toUInt()
      val elem = StypeInstruction(rsrd=0, mod=ELEM, offset = StypeBaseAddress.X, LOAD).toUInt()
      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)))
      dut.io.instr.poke(dof)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      //Simulates a DOF load followed by an elem load
      for(ijk <- ijk) {
        expectIJK(dut, ijk(0), ijk(1), ijk(2))
        dut.clock.step()
      }
      dut.io.instr.poke(elem)
      for(ijk <- ijk) {
        expectIJK(dut, ijk(0), ijk(1), ijk(2))
        dut.clock.step()
      }
    }
  }
  it should "keep the output constant when mod is SEL, FCN, EDN1 or EDN2" in {
    test(new IJKGeneratorFSM) { dut =>
      val sel = StypeInstruction(0, SEL, StypeBaseAddress.X, LOAD).toUInt()
      val fcn = StypeInstruction(0, FCN, StypeBaseAddress.X, LOAD).toUInt()
      val edn1 = StypeInstruction(0, EDN1, StypeBaseAddress.X, LOAD).toUInt()
      val edn2 = StypeInstruction(0, EDN2, StypeBaseAddress.X, LOAD).toUInt()

      val instrs = Seq(sel, fcn, edn1, edn2)

      dut.io.neighbour.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        dut.clock.step()
        expectIJK(dut, 0,0,0)
        dut.clock.step()
        expectIJK(dut, 0,0,0)
        //Stepping twice to verify that output is kept constant
      }
    }
  }
  it should "increment and keep its value when moving to Estart" in {
    test(new IJKGeneratorFSM) { dut =>
      val dof = StypeInstruction(rsrd = 0, mod=DOF, offset = StypeBaseAddress.X, LOAD).toUInt()
      val elem = StypeInstruction(rsrd=0, mod=ELEM, offset = StypeBaseAddress.X, LOAD).toUInt()
      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0))).last
      val instrs = Seq(dof, elem)

      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      for(instr <- instrs) {
        //Poke and step until final output. Verify those values
        dut.io.instr.poke(instr)
        dut.io.threadState.poke(sLoad)
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, ijk(0), ijk(1), ijk(2))

        dut.clock.step()
        dut.io.threadState.poke(sEstart)
        //Nothing should be valid anymore
        dut.io.neighbour.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)

        dut.clock.step(10) //It should saturate the output with the value that other thread should start at
        val out = nextIJK(ijk)
        expectIJK(dut, out(0), out(1), out(2))
        dut.io.threadState.poke(sStore) //To move back to output state without loading values
        //outputs still shouldn't be valid
        dut.io.neighbour.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "not increment until leaving sOutput when using neighbour gen" in {
    test(new IJKGeneratorFSM).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val sel = StypeInstruction(0, SEL, StypeBaseAddress.X, LOAD).toUInt()
      val fcn = StypeInstruction(0, FCN, StypeBaseAddress.X, LOAD).toUInt()
      val edn1 = StypeInstruction(0, EDN1, StypeBaseAddress.X, LOAD).toUInt()
      val edn2 = StypeInstruction(0, EDN2, StypeBaseAddress.X, LOAD).toUInt()
      val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)), elems=2).last
      val instrs = Seq(sel, fcn, edn1, edn2)

      dut.io.neighbour.ready.poke(true.B)
      for(instr <- instrs) {
        //Poke and step until final output. Verify those values
        dut.io.instr.poke(instr)
        dut.io.threadState.poke(sLoad)
        dut.clock.step(VREG_SLOT_WIDTH)
        expectIJK(dut, 0, 0, 0)

        dut.clock.step()
        dut.io.threadState.poke(sEstart)
        //Nothing should be valid anymore
        dut.io.neighbour.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)
        //It should saturate the output with the value that other thread should start at
        dut.clock.step(10)
        expectIJK(dut, ijk(0), ijk(1), ijk(2))
        dut.io.threadState.poke(sStore) //To move back to output state without loading values
        //outputs still shouldn't be valid
        dut.io.neighbour.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)
        dut.clock.step()
      }
    }
  }

  it should "load new values when processing DOF or ELEM" in {
    val dof = StypeInstruction(rsrd = 0, mod=DOF, offset = StypeBaseAddress.X, LOAD).toUInt()
    val elem = StypeInstruction(rsrd=0, mod=ELEM, offset = StypeBaseAddress.X, LOAD).toUInt()
    val ijk = genIJKmultiple(start = Some(Array(0,0,0,0))).last
    val instrs = Seq(dof, elem)

    test(new IJKGeneratorFSM) { dut =>
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      //Verify that both instructions go through the same range of values
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, 0,0,0)
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, ijk(0), ijk(1), ijk(2))
        dut.clock.step()
      }
      //Increment up to maximum value
      dut.io.threadState.poke(sEstart)
      dut.clock.step(10)
      //Get the final value that other thread will increment to
      val out = peekIJK(dut)
      val ijkNext = genIJKmultiple(start = Some(out), elems=XREG_DEPTH+1).last
      pokeIJK(dut, ijkNext(0), ijkNext(1), ijkNext(2), ijkNext(3))

      //Go through iterations in sStore as well
      dut.io.threadState.poke(sStore)
      dut.clock.step()
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, 0,0,0)
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, ijk(0), ijk(1), ijk(2))
        dut.clock.step()
      }
      dut.io.threadState.poke(sPend)
      dut.clock.step() //State is now sWait
      dut.io.threadState.poke(sLoad)
      dut.clock.step() //Should've now incremented to that value
      val ijkNew = genIJKmultiple(start = Some(ijkNext))
      val first = ijkNew(0)
      val last = ijkNew.last
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, first(0), first(1), first(2))
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, last(0), last(1), last(2))
        dut.clock.step()
      }
    }
  }

  it should "load new values when processing SEL, FCN, EDN1 and EDN2" in {
    val sel = StypeInstruction(0, SEL, StypeBaseAddress.X, LOAD).toUInt()
    val fcn = StypeInstruction(0, FCN, StypeBaseAddress.X, LOAD).toUInt()
    val edn1 = StypeInstruction(0, EDN1, StypeBaseAddress.X, LOAD).toUInt()
    val edn2 = StypeInstruction(0, EDN2, StypeBaseAddress.X, LOAD).toUInt()
    val ijk = genIJKmultiple(start = Some(Array(0,0,0,0)), elems=2).last

    val instrs = Seq(sel, fcn, edn1, edn2)
    test(new IJKGeneratorFSM).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      //Verify that both instructions go through the same range of values
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, 0,0,0)
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, 0,0,0) //Output shouldn't change when processing these instructions
        dut.clock.step()
      }
      //Increment up to maximum value
      dut.io.threadState.poke(sEstart)
      dut.clock.step(10)
      //Get the final value that other thread will increment to
      val out = peekIJK(dut)
      val ijkNext = genIJKmultiple(start = Some(out), elems=2).last
      pokeIJK(dut, ijkNext(0), ijkNext(1), ijkNext(2), ijkNext(3))

      //Go through iterations in sStore as well
      dut.io.threadState.poke(sStore)
      dut.clock.step()
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, 0,0,0)
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, 0,0,0)
        dut.clock.step()
      }
      dut.io.threadState.poke(sPend)
      dut.clock.step() //State is now sWait
      dut.io.threadState.poke(sLoad)
      dut.clock.step() //Should've now incremented to load value
      for(instr <- instrs) {
        dut.io.instr.poke(instr)
        expectIJK(dut, ijkNext(0), ijkNext(1), ijkNext(2))
        dut.clock.step(VREG_SLOT_WIDTH-1)
        expectIJK(dut, ijkNext(0), ijkNext(1), ijkNext(2))
        dut.clock.step()
      }
    }
  }

  it should "load values when moving from Wait1 state" in {
    test(new IJKGeneratorFSM) { dut =>
      dut.io.threadState.poke(sWait1)
      dut.clock.step()
      pokeIJK(dut, 0, 4, 0, 0)
      dut.io.threadState.poke(sLoad)
      dut.clock.step()
      expectIJK(dut, 0, 4, 0)
    }
  }

  it should "not increment when outside an output state" in {
    test(new IJKGeneratorFSM) { dut =>
      dut.clock.step()
      dut.io.neighbour.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      for(i <- 0 until 10) {
        dut.clock.step()
        expectIJK(dut, 0, 0, 0)
        dut.io.neighbour.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)
      }
    }
  }
}
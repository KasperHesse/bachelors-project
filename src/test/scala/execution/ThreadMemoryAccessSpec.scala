package execution

import chisel3._
import chiseltest._
import chisel3.experimental.BundleLiterals._
import memory.{IJKBundle, genIJKmultiple}
import execution.RegisterFileType._
import execution.StypeBaseAddress._
import execution.StypeMod._
import execution.StypeLoadStore._
import utils.Config.{ELEMS_PER_VSLOT, NUM_MEMORY_BANKS, SUBVECTORS_PER_VREG, VREG_SLOT_WIDTH}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class ThreadMemoryAccessSpec extends AnyFlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Thread Memory Access Module"

  def expectNeighbour(dut: ThreadMemoryAccess, baseAddr: StypeBaseAddress.Type, ijk: Array[Int], mod: StypeMod.Type, pad: Boolean, valid: Boolean = true): Unit = {
    dut.io.neighbour.valid.expect(valid.B)
    dut.io.neighbour.bits.baseAddr.expect(baseAddr)
    dut.io.neighbour.bits.ijk.expect((new IJKBundle).Lit(_.i -> ijk(0).U, _.j -> ijk(1).U, _.k -> ijk(2).U))
    dut.io.neighbour.bits.mod.expect(mod)
    dut.io.neighbour.bits.pad.expect(pad.B)
  }

  def expectEdof(dut: ThreadMemoryAccess, baseAddr: StypeBaseAddress.Type, ijk: Array[Int], mod: StypeMod.Type, pad: Boolean, valid: Boolean = true): Unit = {
    dut.io.edof.valid.expect(valid.B)
    dut.io.edof.bits.baseAddr.expect(baseAddr)
    dut.io.edof.bits.ijk.expect((new IJKBundle).Lit(_.i -> ijk(0).U, _.j -> ijk(1).U, _.k -> ijk(2).U))
    dut.io.edof.bits.mod.expect(mod)
    dut.io.edof.bits.pad.expect(pad.B)
  }

  def expectReadQueue(dut: ThreadMemoryAccess, iter: Int, reg: Int, rf: RegisterFileType.Type): Unit = {
    dut.io.readQueue.valid.expect(true.B)
    dut.io.readQueue.bits.iter.expect(iter.U)
    dut.io.readQueue.bits.rd.reg.expect(reg.U)
    dut.io.readQueue.bits.rd.rf.expect(rf)
  }

  def expectVec(dut: ThreadMemoryAccess, baseIndex: Int, baseAddr: StypeBaseAddress.Type, valid: Boolean = true): Unit = {
    for(i <- 0 until NUM_MEMORY_BANKS) {
      dut.io.vec.bits.indices(i).expect((baseIndex+i).U)
      dut.io.vec.bits.baseAddr.expect(baseAddr)
      dut.io.vec.valid.expect(valid.B)
    }
  }

  it should "not start transmitting DOF until ready goes high" in {
    test(new ThreadMemoryAccess(sim=true)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val rd = 0
      val instr = StypeInstruction(rsrd=rd, mod=DOF, baseAddr=Q, ls=LOAD)

      //Keep all ready signals low for now
      dut.io.vec.ready.poke(false.B)
      dut.io.edof.ready.poke(false.B)
      dut.io.neighbour.ready.poke(false.B)
      dut.io.readQueue.ready.poke(false.B)
      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      //edof isn't ready - read queue shouldn't be valid
      dut.io.readQueue.valid.expect(false.B)
      dut.io.edof.valid.expect(true.B)
      dut.io.readQueue.bits.rd.reg.expect(0.U)

      dut.clock.step(5) //Rd should still be 0 since no increments are performed
      dut.io.readQueue.bits.rd.reg.expect(0.U)
      dut.io.edof.ready.poke(true.B)
      dut.clock.step(3) //Rd should now update
      dut.io.readQueue.valid.expect(true.B)
      dut.io.readQueue.bits.rd.reg.expect(1.U)
    }
  }

  it should "generate outputs for a DOF instruction" in {
    test(new ThreadMemoryAccess(sim=true)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val rd = 2
      val instr = StypeInstruction(rsrd=rd, mod=DOF, baseAddr=Q, ls=LOAD)
      val IJK = genIJKmultiple(start=Some(Array(0,0,0,0)))

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      for(i <- 0 until IJK.length*SUBVECTORS_PER_VREG) {
        val j = i/SUBVECTORS_PER_VREG //outputs should only update on every third iteration
        val ijk = IJK(j)

        if(i % SUBVECTORS_PER_VREG==0) expectEdof(dut, baseAddr=Q, ijk=ijk, pad=false, mod=DOF)
        expectReadQueue(dut, iter=0, reg=rd*VREG_SLOT_WIDTH + j, rf=VREG)
        dut.io.vec.valid.expect(false.B)
        dut.io.neighbour.valid.expect(false.B)

        val finalCycle = i == (IJK.length*SUBVECTORS_PER_VREG-1)
        dut.io.finalCycle.expect(finalCycle.B)
        if(!finalCycle) dut.clock.step()
        //We must poke edof ready low after 0th and high after 2nd to prevent ijk generator from updating too fast
        if(i % SUBVECTORS_PER_VREG == 0) {
          dut.io.edof.ready.poke(false.B)
        } else if (i % SUBVECTORS_PER_VREG == SUBVECTORS_PER_VREG-1) {
          dut.io.edof.ready.poke(true.B)
        }
      }
      dut.io.threadState.poke(ThreadState.sEstart)
      dut.clock.step()

      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
    }
  }

  it should "generate outputs for a VEC instruction" in {
    test(new ThreadMemoryAccess(sim=true)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val rd = 1
      val instr = StypeInstruction(rsrd=rd, mod=VEC, baseAddr=R, ls=LOAD)
      val baseIndices = Seq.range(0, ELEMS_PER_VSLOT, NUM_MEMORY_BANKS)

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      for(i <- baseIndices.indices) {
        val baseIndex = baseIndices(i)
        expectVec(dut, baseIndex, baseAddr=R)
        expectReadQueue(dut, iter=0, reg=(rd*VREG_SLOT_WIDTH + i/SUBVECTORS_PER_VREG), rf=VREG)
        dut.io.edof.valid.expect(false.B)
        dut.io.neighbour.valid.expect(false.B)

        val finalCycle = i == (baseIndices.length-1)
        dut.io.finalCycle.expect(finalCycle.B)
        if(!finalCycle) dut.clock.step()
      }
      dut.io.threadState.poke(ThreadState.sEstart)
      dut.clock.step()

      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
    }
  }

  it should "generate outputs for an ELEM instruction" in {
    test(new ThreadMemoryAccess(sim=true)).withAnnotations(Seq(WriteVcdAnnotation)) {dut =>
      val instr = StypeInstruction(rsrd=3, mod=ELEM, baseAddr=XPHYS, ls=LOAD)
      val IJK = genIJKmultiple(start=Some(Array(0,0,0,0)))

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      //Should generate multiple outputs
      for(i <- IJK.indices) {
        val ijk = IJK(i)
        expectNeighbour(dut, baseAddr=XPHYS, ijk, mod=ELEM, pad=false)
        expectReadQueue(dut, ijk(3), reg=3, rf=XREG)
        dut.io.vec.valid.expect(false.B)
        dut.io.edof.valid.expect(false.B)

        val finalCycle = (i == IJK.length-1)
        dut.io.finalCycle.expect(finalCycle.B)
        if (!finalCycle) dut.clock.step() //Must poke new threadstate before stepping the last time around
      }
      dut.io.threadState.poke(ThreadState.sEstart)
      dut.clock.step()

      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)

    }
  }

  it should "generate outputs for a FCN instruction" in {
    test(new ThreadMemoryAccess(sim=true)) { dut =>
      //Drive a ld.sel instruction. Observe outputs at ijk=0,0,0 and correct rd (and only one)
      //When finalCycle is asserted, move to sEstart, all valid signals should go low
      val instr = StypeInstruction(rsrd = 2, mod=FCN, baseAddr=X, ls = LOAD)

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      //Should start now
      expectNeighbour(dut, baseAddr=X, ijk = Array(0,0,0), mod=FCN, pad=false)
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(false.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)

      //One more clock cycle where readQueue outputs but ijk doesn't
      dut.clock.step()
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(true.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.threadState.poke(ThreadState.sEstart)

      dut.clock.step()
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
    }
  }

  it should "generate outputs for an EDN1 instruction" in {
    test(new ThreadMemoryAccess(sim=true)) { dut =>
      //Drive a ld.sel instruction. Observe outputs at ijk=0,0,0 and correct rd (and only one)
      //When finalCycle is asserted, move to sEstart, all valid signals should go low
      val instr = StypeInstruction(rsrd = 2, mod=EDN1, baseAddr=X, ls = LOAD)

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      //Should start now
      expectNeighbour(dut, baseAddr=X, ijk = Array(0,0,0), mod=EDN1, pad=false)
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(false.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)

      //One more clock cycle where readQueue outputs but ijk doesn't
      dut.clock.step()
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(true.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.threadState.poke(ThreadState.sEstart)

      dut.clock.step()
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
    }
  }

  it should "generate outputs for an EDN2 instruction" in {
    test(new ThreadMemoryAccess(sim=true)) { dut =>
      //Drive a ld.sel instruction. Observe outputs at ijk=0,0,0 and correct rd (and only one)
      //When finalCycle is asserted, move to sEstart, all valid signals should go low
      val instr = StypeInstruction(rsrd = 2, mod=EDN2, baseAddr=X, ls = LOAD)

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.clock.step()

      //Should start now
      expectNeighbour(dut, baseAddr=X, ijk = Array(0,0,0), mod=EDN2, pad=false)
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(false.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)

      //One more clock cycle where readQueue outputs but ijk doesn't
      dut.clock.step()
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(true.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.threadState.poke(ThreadState.sEstart)

      dut.clock.step()
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
    }
  }


  it should "generate outputs for a SEL instruction" in {
    test(new ThreadMemoryAccess(sim=true)) { dut =>
      //Drive a ld.sel instruction. Observe outputs at ijk=0,0,0 and correct rd (and only one)
      //When finalCycle is asserted, move to sEstart, all valid signals should go low
      val instr = StypeInstruction(rsrd = 2, mod=SEL, baseAddr=X, ls = LOAD)

      dut.io.vec.ready.poke(true.B)
      dut.io.edof.ready.poke(true.B)
      dut.io.neighbour.ready.poke(true.B)
      dut.io.readQueue.ready.poke(true.B)

      dut.io.threadState.poke(ThreadState.sLoad)
      dut.io.instr.poke(instr)
      dut.io.finalCycle.expect(false.B)
      dut.clock.step()

      //Should start now
      expectNeighbour(dut, baseAddr=X, ijk = Array(0,0,0), mod=SEL, pad=false)
      expectReadQueue(dut, iter=0, reg=2, rf = XREG)
      dut.io.finalCycle.expect(true.B)
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)

      dut.io.threadState.poke(ThreadState.sEstart)
      dut.clock.step()
      dut.io.vec.valid.expect(false.B)
      dut.io.edof.valid.expect(false.B)
      dut.io.neighbour.valid.expect(false.B)
      dut.io.readQueue.valid.expect(false.B)
      dut.io.finalCycle.expect(false.B)
    }
  }
}

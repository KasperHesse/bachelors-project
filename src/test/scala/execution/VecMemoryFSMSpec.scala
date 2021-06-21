package execution

import chiseltest.ChiselScalatestTester
import org.scalatest.{FlatSpec, Matchers}

import chisel3._
import chisel3.experimental.BundleLiterals._
import chiseltest._
import org.scalatest.{FlatSpec, Matchers}
import execution.ThreadState._
import StypeLoadStore._
import StypeMod._
import InstructionFMT._
import chiseltest.internal.WriteVcdAnnotation
import chiseltest.experimental.TestOptionBuilder._
import memory.{IJKBundle, genIJK, genIJKmultiple, nextIJK}
import utils.Fixed._
import utils.Config._

class VecMemoryFSMSpec extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "Vector memory access FSM"

  def expectIndices(dut: VecMemoryFSM, baseIndex: Int): Unit = {
    for(i <- 0 until NUM_MEMORY_BANKS) {
      dut.io.vec.bits.indices(i).expect((baseIndex+i).U)
    }
  }

  it should "keep valid low when not in sStore or sLoad" in {
    test(new VecMemoryFSM) {dut =>
      val stateList = Seq(sIdle, sEstart, sExec, sEend, sPend, sWait1, sWait2)
      for(state <- stateList) {
        dut.io.threadState.poke(state)
        dut.clock.step()
        dut.io.vec.valid.expect(false.B)
      }
    }
  }

  it should "output multiple values in a row" in {
    test(new VecMemoryFSM).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      val instr = StypeInstruction(rsrd = 0, mod=VEC, baseAddr = StypeBaseAddress.XPHYS, LOAD)
      val indices = Seq.range(start= 0, end=VREG_SLOT_WIDTH*VREG_DEPTH, step=NUM_MEMORY_BANKS)
      dut.io.instr.poke(instr)
      dut.io.vec.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      //Simulates two sequential VEC loads
      for(index <- indices) {
        expectIndices(dut, index)
        dut.clock.step()
      }
      for(index <- indices) {
        expectIndices(dut, index)
        dut.clock.step()
      }
    }
  }


  it should "increment and keep its value when moving to Estart" in {
    test(new VecMemoryFSM) { dut =>
      val instr = StypeInstruction(rsrd = 0, mod=VEC, baseAddr = StypeBaseAddress.XPHYS, LOAD)
      val indices = Seq.range(start= 0, end=VREG_SLOT_WIDTH*VREG_DEPTH, step=NUM_MEMORY_BANKS)

      dut.io.vec.ready.poke(true.B)
      //Poke and step until final output. Verify those values
      dut.io.instr.poke(instr)
      dut.io.threadState.poke(sLoad)
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, indices.last)

      dut.clock.step()
      dut.io.threadState.poke(sEstart)
      //Should not be valid
      dut.io.vec.valid.expect(false.B)

      dut.clock.step(30) //It should saturate the output with the value that other thread should start at
      expectIndices(dut, indices.last + 8)
      dut.io.threadState.poke(sStore) //To move back to output state without loading values
      //outputs still shouldn't be valid
      dut.io.vec.valid.expect(false.B)
      dut.clock.step()
    }
  }


  it should "load new values when processing the next iteration" in {
    val instr = StypeInstruction(rsrd = 0, mod=VEC, baseAddr = StypeBaseAddress.XPHYS, LOAD)
    val indices = Seq.range(start= 0, end=VREG_SLOT_WIDTH*VREG_DEPTH, step=NUM_MEMORY_BANKS)

    test(new VecMemoryFSM).withAnnotations(Seq(WriteVcdAnnotation)) { dut =>
      dut.io.vec.ready.poke(true.B)
      dut.io.threadState.poke(sLoad)
      //Verify
      dut.io.instr.poke(instr)
      expectIndices(dut, 0)
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, indices.last)
      dut.io.finalCycle.expect(true.B)
      dut.clock.step()
      //Increment up to maximum value
      dut.io.threadState.poke(sEstart)
      dut.clock.step(30)
      //Get the final value that other thread will increment to
      expectIndices(dut, indices.last+8)

      //Go through iterations in sStore as well
      dut.io.threadState.poke(sStore)
      dut.clock.step()
      expectIndices(dut, 0)
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, indices.last)

      dut.io.threadState.poke(sPend)
      dut.clock.step() //State is now sWait
      dut.io.threadState.poke(sLoad)
      dut.clock.step() //Should've now incremented to that new values

      expectIndices(dut, 2*VREG_SLOT_WIDTH*VREG_DEPTH)
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, 3*VREG_SLOT_WIDTH*VREG_DEPTH-8)
      dut.clock.step()
    }
  }

  it should "load values when moving from the Wait1 state" in {
    val instr = StypeInstruction(rsrd = 0, mod=VEC, baseAddr = StypeBaseAddress.XPHYS, LOAD)
    val indices = Seq.range(start=VREG_SLOT_WIDTH*VREG_DEPTH, end=2*VREG_SLOT_WIDTH*VREG_DEPTH, step=NUM_MEMORY_BANKS)

    test(new VecMemoryFSM) { dut =>
      dut.io.instr.poke(instr)
      dut.io.threadState.poke(sWait1)
      dut.io.vec.ready.poke(true.B)
      dut.clock.step()

      dut.io.threadState.poke(sLoad)
      dut.clock.step()
      expectIndices(dut, indices(0))
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, indices.last)
      dut.io.finalCycle.expect(true.B)
      dut.clock.step()

      //Increment up to maximum value
      dut.io.threadState.poke(sEstart)
      dut.clock.step(30)
      expectIndices(dut, indices.last+8)

      //Go through iterations in sStore as well
      dut.io.threadState.poke(sStore)
      dut.clock.step()
      expectIndices(dut, indices(0))
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, indices.last)

      dut.io.threadState.poke(sPend)
      dut.clock.step() //State is now sWait
      dut.io.threadState.poke(sLoad)
      dut.clock.step() //Should've now incremented to that new values

      expectIndices(dut, 3*VREG_SLOT_WIDTH*VREG_DEPTH)
      dut.clock.step(VREG_SLOT_WIDTH*SUBVECTORS_PER_VREG-1)
      expectIndices(dut, 4*VREG_SLOT_WIDTH*VREG_DEPTH-8)
      dut.clock.step()
    }
  }
}

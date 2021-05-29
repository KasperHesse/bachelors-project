package pipeline

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import memory.{AddressGenProducerIO, IJKgenerator, IJKgeneratorBundle, IJKgeneratorConsumerIO, ReadQueueBundle}
import pipeline.StypeMod._
import utils.Config._
import pipeline.ThreadState._

class MemoryAccessFSMIO extends Bundle {
  /** Instruction bits from Thread module */
  val instr = Input(UInt(32.W))
  /** Current state of parent Thread module */
  val threadState = Input(ThreadState())
  /** Explicit reset signal that resets the IJK generator and control FSM. Not the same as the Chisel-inferred reset signal... */
  val reset = Input(Bool())
  /** Values used when performing .dof operations that go through the EDOF generator */
  val edof = Decoupled(new IJKgeneratorConsumerIO)
  /** Values used when performing .elem, .fcn, .edn1, .edn2 and .sel operations that go to neighbour generator */
  val neighbour = Decoupled(new IJKgeneratorConsumerIO)
  /** I,J,K values passed to other thread */
  val ijkOut = Output(new IJKgeneratorBundle)
  /** I,J,K values input from other thread */
  val ijkIn = Flipped(new IJKgeneratorBundle)
}

/**
 * This module contains the FSM used to control the IJK generation module and perform interfacing with memory stage
 */
class IJKGeneratorFSM extends Module {
  import MemoryAccessFSMState._
  val io = IO(new MemoryAccessFSMIO)

  /// -- MODULES ---
  val ijkGenerator = Module(new IJKgenerator)

  // --- REGISTERS AND WIRES ---
  /** Current state */
  val state = RegInit(sOutput)
  /** Number of values that have been output on this instruction */
  val cnt = RegInit(0.U(4.W))
  /** Registered version of current instruction. Only updated when current threadState is sLoad */
  val regSinstr = RegEnable(io.instr.asTypeOf(new StypeInstruction), io.threadState === sLoad || io.threadState === sStore)
  /** Handle to most recent valid Stype instruction. If thread state is not sStore or sLoad, keeps the previous instruction saved */
  val Sinstr = Mux(io.threadState === sLoad || io.threadState === sStore, io.instr.asTypeOf(new StypeInstruction), regSinstr)
  /** Number of values total to output on this instruction */
  val cntMax = WireDefault(0.U(4.W))
  /** Ready signal received from the connected consumer module */
  val ready = Wire(Bool())
  /** True whenever the current state is one where an output should be generated */
  val outputState = (io.threadState === sLoad || io.threadState === sStore) && state === sOutput
  /** Valid signal for connected consumer module */
  val valid = outputState
  /** Load signal going into IJK generator */
  val load = WireDefault(false.B)
  /** Restart signal going into IJK generator */
  val restart = WireDefault(false.B)
  /** Next signal going into IJK generator */
  val next = WireDefault(false.B)

  // --- LOGIC ---
  //Connect ready signal from correct consumer
  when(Sinstr.mod === DOF) {
    ready := io.edof.ready
  } .otherwise {
    ready := io.neighbour.ready
  }

  //Set cntMax based on instruction type
  when(Sinstr.mod === VEC || Sinstr.mod === DOF || Sinstr.mod === ELEM) {
    cntMax := (VREG_SLOT_WIDTH-1).U
  } .otherwise { //SEL, FCN, EDN1, EDN2.
    cntMax := 0.U
  }

  //Next state and cnt logic
  switch(state) {
    is(sOutput) {
      when(ready && cnt < cntMax && outputState) {
        cnt := cnt + 1.U
      } .elsewhen(ready && cnt === cntMax && outputState) {
        cnt := 0.U
      }
      when(io.threadState === ThreadState.sEstart) { //end of load operation
        state := sCalcNext
        cnt := 0.U
      }
      when(io.threadState === ThreadState.sPend || io.threadState === sWait1) { //End of store / start of load
        state := sWait
        cnt := 0.U
      }
    }
    is(sCalcNext) {
      cnt := cnt + 1.U
      when(cnt === cntMax) {
        cnt := 0.U
        state := sWait
      }
    }
    is(sWait) {
      when(io.threadState === sStore || io.threadState === sLoad) {
        state := sOutput
      }
    }
  }

  //IJK generator signal logic
  switch(state) {
    is(sOutput) {
      when(ready && cnt === cntMax && outputState) {
        restart := true.B
      } .elsewhen(ready && cnt < cntMax && outputState) {
        next := true.B
      }
      when(io.threadState === sEstart) {
        restart := true.B
      }
    }
    is(sCalcNext) {
      when(cnt <= cntMax) {
        next := true.B
      }
    }
    is(sWait) {
      when(io.threadState === sStore) {
        restart := true.B
      } .elsewhen(io.threadState === sLoad) {
        load := true.B
      }
    }
  }

  //Connections
  ijkGenerator.io.in := io.ijkIn
  ijkGenerator.io.ctrl.load := load
  ijkGenerator.io.ctrl.restart := restart
  ijkGenerator.io.ctrl.next := next

  io.ijkOut := ijkGenerator.io.out
  io.neighbour.bits.ijk := ijkGenerator.io.out.ijk
  io.edof.bits.ijk := ijkGenerator.io.out.ijk

  io.edof.bits.baseAddr := Sinstr.baseAddr
  io.neighbour.bits.baseAddr := Sinstr.baseAddr
  io.neighbour.bits.mod := Sinstr.mod
  io.edof.bits.mod := Sinstr.mod
  io.neighbour.bits.pad := ijkGenerator.io.ctrl.pad
  io.edof.bits.pad := ijkGenerator.io.ctrl.pad

  //All output valid signals default to false. Overridden by when statement below
  io.edof.valid := false.B
  io.neighbour.valid := false.B
  when(Sinstr.mod === DOF) {
    io.edof.valid := valid
  } .otherwise {
    io.neighbour.valid := valid
  }

}

object MemoryAccessFSMState extends ChiselEnum {
  val sOutput = Value(0.U)
  val sCalcNext = Value(1.U)
  val sWait = Value(2.U)
}

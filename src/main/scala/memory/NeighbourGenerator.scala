package memory

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import pipeline.StypeMod._
import utils.Config._

/**
 * I/O ports for the [[NeighbourGenerator]] module.
 */
class NeighbourGeneratorIO extends Bundle {
  val in = Flipped(Decoupled(new IJKgeneratorConsumerIO))
  val indexGen = Decoupled(new IndexGeneratorProducerIO)
}

/**
 * This module takes a set of IJK-values and the current Stype modifier, and outputs the indices of the neighbouring elements.
 * It may either calculate face or edge neighbours, or the input coordinate tuple if [[pipeline.StypeMod.SEL]] is given as the modifier.
 * If the input modifier is not one of [[SEL]], [[FCN]], [[EDN1]] or [[EDN2]], the module will stay in the idle state
 */
class NeighbourGenerator extends Module {
  import NeighbourGeneratorState._
  val io = IO(new NeighbourGeneratorIO)

  //The neighbour generator should latch in new values when input is valid and consumer is ready
  //Should use the StypeMod / operation type to choose how to generate these values
  //Once latched in, output should be valid immediatedly after.
  // --- REGISTERS ---
  /** Current state of the neighbour generator */
  val state = RegInit(sIdle)
  /** Pipeline register. Updated whenever producer has valid data and module is in idle state */
  val in = RegEnable(io.in.bits, io.in.valid && state === sIdle)

  // --- SIGNALS AND WIRES ---
  /** IJK values being output to index generator */
  val ijk = WireDefault(VecInit(
    Seq.fill(NUM_MEMORY_BANKS)((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U)))
  )
  /** Valid bits being output to index generator */
  val validIjk = WireDefault(VecInit(Seq.fill(NUM_MEMORY_BANKS)(false.B)))
  //TODO this module probably does not use the 'pad' input from IJK generator. Is that correct?

  // --- LOGIC ---
  //Next state logic
  switch(state) {
    is(sIdle) {
      when(io.in.valid) {
        when(io.in.bits.mod === FCN) {
          state := sFcn_1
        } .elsewhen(io.in.bits.mod === EDN1) {
          state := sEdn1_1
        } .elsewhen(io.in.bits.mod === EDN2) {
          state := sEdn2_1
        } .elsewhen(io.in.bits.mod === SEL) {
          state := sSel
        }
      }
    }
    is(sFcn_1) {
      state := Mux(io.indexGen.ready, sFcn_2, state)
    }
    is(sFcn_2) {
      state := Mux(io.indexGen.ready, sIdle, state)
    }
    is(sEdn1_1) {
      state := Mux(io.indexGen.ready, sEdn1_2, state)
    }
    is(sEdn1_2) {
      state := Mux(io.indexGen.ready, sIdle, state)
    }
    is(sEdn2_1) {
      state := Mux(io.indexGen.ready, sEdn2_2, state)
    }
    is(sEdn2_2) {
      state := Mux(io.indexGen.ready, sIdle, state)
    }
    is(sSel) {
      state := Mux(io.indexGen.ready, sIdle, state)
    }
  }

  /**
   * Generates hardware to compute an ijk-pair offset from the input ijk-values.
   * Sets the new ijk-values and toggles the valid flag for the given index
   * @param di Offset in the i(x)-direction
   * @param dj Offset in the j(y)-direction
   * @param dk Offset in the k(z)-direction
   * @param index Which index of the output ijk-vector should be set
   */
  def IJKoffset(di: Int, dj: Int, dk: Int, index: Int): Unit = {
    require(-1 <= di && di <= 1, "di must be between -1 and 1")
    require(-1 <= dk && dk <= 1, "dj must be between -1 and 1")
    require(-1 <= dj && dj <= 1, "dk must be between -1 and 1")
    require(0 <= index && index < NUM_MEMORY_BANKS, s"index must be between 0 and ${NUM_MEMORY_BANKS-1}")
    ijk(index).i := (in.ijk.i.asSInt() + di.S).asUInt()
    ijk(index).j := (in.ijk.j.asSInt() + dj.S).asUInt()
    ijk(index).k := (in.ijk.k.asSInt() + dk.S).asUInt()
    validIjk(index) := true.B
  }

  //Output logic
  switch(state) {
    is(sFcn_1) {
      IJKoffset(0, 0, 1, 0)
      IJKoffset(0, 1, 0, 1)
      IJKoffset(1, 0, 0, 2)
    }
    is(sFcn_2) {
      IJKoffset(0,0,-1,0)
      IJKoffset(0,-1,0,1)
      IJKoffset(-1,0,0,2)
    }
    is(sEdn1_1) {
      IJKoffset(0,1,1,0)
      IJKoffset(1,0,1,1)
      IJKoffset(1,0,0,2)
    }
    is(sEdn1_2) {
      IJKoffset(0,-1,1,0)
      IJKoffset(-1,0,1,1)
      IJKoffset(-1,0,0,2)
    }
    is(sEdn2_1) {
      IJKoffset(0,1,-1,0)
      IJKoffset(1,0,-1,1)
      IJKoffset(0,1,0,2)
    }
    is(sEdn2_2) {
      IJKoffset(0,-1,-1,0)
      IJKoffset(-1,0,-1,1)
      IJKoffset(0,-1,0,2)
    }
    is(sSel) {
      IJKoffset(0,0,0,0)
    }
  }

  //Output logic
  io.in.ready := state === sIdle

  io.indexGen.valid := state =/= sIdle
  io.indexGen.bits.ijk := ijk
  io.indexGen.bits.validIjk := validIjk

  io.indexGen.bits.baseAddr := in.baseAddr
  io.indexGen.bits.ls := in.ls
}

object NeighbourGeneratorState extends ChiselEnum {
  val sIdle, sFcn_1, sFcn_2, sEdn1_1, sEdn1_2, sEdn2_1, sEdn2_2, sSel = Value
}
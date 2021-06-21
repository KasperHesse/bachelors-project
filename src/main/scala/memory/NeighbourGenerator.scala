package memory

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import execution.StypeMod._
import utils.Config._

/**
 * I/O ports for the [[NeighbourGenerator]] module.
 */
class NeighbourGeneratorIO extends Bundle {
  val in = Flipped(Decoupled(new IJKgeneratorConsumerIO))
  val indexGen = Decoupled(new NeighbourGenIndexGenIO)
}

/**
 * This module takes a set of IJK-values and the current Stype modifier, and outputs the indices of the neighbouring elements.
 * It may either calculate face or edge neighbours, or it will output the given input coordinate tuple if [[execution.StypeMod.SEL]] or [[execution.StypeMod.ELEM]] is given as the modifier.
 * If the input modifier is not one of [[SEL]], [[ELEM]], [[FCN]], [[EDN1]] or [[EDN2]], the module will stay in the idle state
 */
class NeighbourGenerator extends Module {
  import NeighbourGeneratorState._
  val io = IO(new NeighbourGeneratorIO)

  val NUM_OUTPUT_PORTS = 3

  //The neighbour generator should latch in new values when input is valid and consumer is ready
  //Should use the StypeMod / operation type to choose how to generate these values
  //Once latched in, output should be valid immediatedly after.
  // --- REGISTERS ---
  /** Current state of the neighbour generator */
  val state = RegInit(sIdleOutput)
  /** Pipeline register. Updated whenever producer has valid data and module is in idle state */
  val in = RegEnable(io.in.bits, io.in.valid && state === sIdleOutput && io.indexGen.ready)

  // --- SIGNALS AND WIRES ---
  /** IJK values being output to index generator */
  val ijk = WireDefault(VecInit(
    Seq.fill(NUM_OUTPUT_PORTS)((new IJKBundle).Lit(_.i -> 0.U, _.j -> 0.U, _.k -> 0.U)))
  )
  /** Valid bits being output to index generator */
  val validIjk = WireDefault(VecInit(Seq.fill(NUM_OUTPUT_PORTS)(false.B)))

  // --- LOGIC ---
  /**
   * Generates hardware to compute an ijk-pair offset from the input ijk-values.
   * Sets the new ijk-values and toggles the valid flag for the given index
   * @param di Offset in the i(x)-direction
   * @param dj Offset in the j(y)-direction
   * @param dk Offset in the k(z)-direction
   * @param index Which index of the output ijk-vector should be set
   */
  def IJKoffset(index: Int, di: Int, dj: Int, dk: Int): Unit = {
    require(-1 <= di && di <= 1, "di must be between -1 and 1")
    require(-1 <= dk && dk <= 1, "dj must be between -1 and 1")
    require(-1 <= dj && dj <= 1, "dk must be between -1 and 1")
    require(0 <= index && index < NUM_MEMORY_BANKS, s"index must be between 0 and ${NUM_MEMORY_BANKS-1}")
    ijk(index).i := (in.ijk.i.asSInt() + di.S).asUInt()
    ijk(index).j := (in.ijk.j.asSInt() + dj.S).asUInt()
    ijk(index).k := (in.ijk.k.asSInt() + dk.S).asUInt()
    validIjk(index) := !in.pad //When pad is true, ijk is always invalid
    //Index generator will perform checks to verify if values are outside boundaries of design domain
  }

  val valid = io.in.valid
  val mod = io.in.bits.mod

  val validOut = RegInit(false.B)


  switch(state) {
    is(sIdleOutput) {
      when(valid && (mod === SEL || mod === ELEM)) {
        //No state necessary
        validOut := true.B
      } .elsewhen(valid && mod === FCN) {
        state := sFcn
        validOut := true.B
      } .elsewhen(valid && mod === EDN1) {
        state := sEdn1
        validOut := true.B
      } .elsewhen(valid && mod === EDN2) {
        state := sEdn2
        validOut := true.B
      }
      when(!valid) {
        validOut := false.B
      }
    }
    is(sFcn) {
      when(io.indexGen.ready) {
        state := sIdleOutput
      }
    }
    is(sEdn1) {
      when(io.indexGen.ready) {
        state := sIdleOutput
      }
    }
    is(sEdn2) {
      when(io.indexGen.ready) {
        state := sIdleOutput
      }
    }
  }

  //Output logic
  when(validOut) {
    switch(in.mod) {
      is(SEL) {
        IJKoffset(0, 0, 0, 0)
      }
      is(ELEM) {
        IJKoffset(0, 0, 0, 0)
      }
      is(FCN) {
        when(state === sFcn) {
          IJKoffset(0, 0, 0, 1)
          IJKoffset(1, 0, 1, 0)
          IJKoffset(2, 1, 0, 0)
        } .otherwise {
          IJKoffset(0, 0, 0, -1)
          IJKoffset(1, 0, -1, 0)
          IJKoffset(2, -1, 0, 0)
        }
      }
      is(EDN1) {
        when(state === sEdn1) {
          IJKoffset(0, 0, 1, 1)
          IJKoffset(1, 1, 0, 1)
          IJKoffset(2, 1, 1, 0)
        } .otherwise {
          IJKoffset(0, 0, -1, 1)
          IJKoffset(1, -1, 0, 1)
          IJKoffset(2, 1, -1, 0)
        }
      }
      is(EDN2) {
        when(state === sEdn2) {
          IJKoffset(0, 0, 1, -1)
          IJKoffset(1, 1, 0, -1)
          IJKoffset(2, -1, 1, 0)
        } .otherwise {
          IJKoffset(0, 0, -1, -1)
          IJKoffset(1, -1, 0, -1)
          IJKoffset(2, -1, -1, 0)
        }
      }
    }
  }

  //Output logic
  io.in.ready := Mux(state === sIdleOutput, io.indexGen.ready, false.B)

  io.indexGen.valid := validOut
  io.indexGen.bits.ijk := ijk
  io.indexGen.bits.validIjk := validIjk

  io.indexGen.bits.baseAddr := in.baseAddr
}

object NeighbourGeneratorState extends ChiselEnum {
  val sIdleOutput, sFcn, sEdn1, sEdn2 = Value
}
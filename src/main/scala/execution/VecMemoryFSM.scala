package pipeline

import chisel3._
import utils.Config._
import chisel3.util._
import memory.AddressGenProducerIO
import pipeline.MemoryAccessFSMState._
import pipeline.ThreadState._

class VecMemoryFSM extends Module {
  val io = IO(new Bundle {
    /** Instruction currently being executed */
    val instr = Input(new StypeInstruction)
    /** Indices used when performing .vec operations that go straight to address generator */
    val vec = Decoupled(new AddressGenProducerIO)
    /** Current state of parent Thread module */
    val threadState = Input(ThreadState())
    /** Current state of the other Thread module */
    val otherThreadState = Input(ThreadState())
    /** Asserted on the final clock cycle of memory loads related to this instruction */
    val finalCycle = Output(Bool())

    val maxIndex = Input(UInt(log2Ceil(NDOFLENGTH + 1).W))
  })

  // --- REGISTERS ---
  val state = RegInit(MemoryAccessFSMState.sOutput)

  /** Number of times the index has been incremented this time around */
  val cnt = RegInit(0.U(log2Ceil(VREG_SLOT_WIDTH * (VREG_DEPTH/NUM_MEMORY_BANKS) + 1).W))
  /** The base index from which the output indices will be constructed */
  val index = RegInit(0.U(log2Ceil(NDOFLENGTH + 1).W))
  /** The index at which to start the next iteration */
  val indexNext = RegInit(ELEMS_PER_VSLOT.U(log2Ceil(NDOFLENGTH + 1).W))
  /** The index value that the counter should reset to on every iteration */
  val indexSaved = RegInit(0.U(log2Ceil(NDOFLENGTH + 1).W))

  // --- SIGNALS ---
  val ready = io.vec.ready

  /** Total number of times the index should be incremented on one instruction */
  val cntMax = (VREG_SLOT_WIDTH * (VREG_DEPTH/NUM_MEMORY_BANKS) - 1).U
  /** High signal when outputs should be driven */
  val outputState = (io.threadState === sLoad || io.threadState === sStore) && state === sOutput && io.instr.fmt === InstructionFMT.STYPE

  //Next state and cnt logic
  switch(state) {
    is(sOutput) {
      when(ready & cnt < cntMax & outputState) {
        cnt := cnt + 1.U
        index := index + 8.U
      }.elsewhen(ready & cnt === cntMax & outputState) {
        cnt := 0.U
        index := indexSaved
      }
      when(io.threadState === sEstart) {
        state := sCalcNext
        cnt := 0.U
      }.elsewhen(io.threadState === ThreadState.sPend || io.threadState === sWait1) { //End of store, start of load operation
        state := sWait
        cnt := 0.U
      }
    }
    is(sCalcNext) {
      cnt := cnt + 1.U
      index := index + 8.U
      when(cnt === cntMax) {
        cnt := 0.U
        state := sWait
      }
    }
    is(sWait) {
      when(io.threadState === sStore) {
        state := sOutput
        index := indexSaved
        indexNext := index + (ELEMS_PER_VSLOT).U
      }.elsewhen(io.threadState === sLoad) {
        state := sOutput
        index := indexNext
        indexSaved := indexNext
      }
    }
  }

  //All outputs are affixed with LSB according to their index
  for (i <- io.vec.bits.indices.indices) {
    io.vec.bits.indices(i) := index | i.U
    io.vec.bits.validIndices(i) := (index | i.U) < io.maxIndex
  }

  io.finalCycle := ready & cnt === cntMax
  io.vec.bits.baseAddr := io.instr.baseAddr
  io.vec.valid := outputState && io.instr.mod === StypeMod.VEC
}
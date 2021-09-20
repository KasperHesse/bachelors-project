package memory

import chisel3._
import chisel3.experimental.ChiselEnum
import chisel3.util._
import execution.StypeMod
import execution.StypeMod._
import utils.Config._
import utils.Fixed.FIXED_WIDTH

class MemWbIO extends Bundle {
  /** Inputs from the memory stage */
  val mem = Flipped(Decoupled(new MemoryWritebackIO))
  /** Inputs from the read queue */
  val readQueue = Flipped(Decoupled(new ReadQueueBundle))
  /** Outputs to the instruction decode stage / register files. Does not use ready/valid signalling */
  val id = Output(new execution.WbIdIO)
}

/**
 * This module accepts values read from the [[OnChipMemory]] and handles them to construct valid packets that will be written
 * back into the vector register files of the connected thread. Implements [[MemWbIO]]
 */
class MemoryWriteback extends Module {
  import MemoryWritebackState._
  val io = IO(new MemWbIO)

  //Whenever mem has valid data, pass that through to ready signal on write queue
  //Update registers whenever mem is valid and we are ready (probably always, combinational from input)

  // --- SIGNALS AND WIRES
  //Shorthands for more easily readable code
  /** Read data vector from memory */
  val rdData = io.mem.bits.rdData
  /** Iteration number from read queue */
  val iteration = io.readQueue.bits.iter
  /** Valid flag from memory */
  val valid = io.mem.valid
  /** S-type modifier from read queue */
  val mod = io.readQueue.bits.mod


  // --- REGISTERS ---
  /** Destination register for operation */
  val rd = RegEnable(io.readQueue.bits.rd, valid)
  /** State register */
  val state = RegInit(sIdleOutput)
  /** Helper register used to track progress throughout writebacks */
  val cnt = RegInit(0.U(3.W))
  /** Write buffer for holding temp. values while the remainder of a result is arriving */
  val writeBuffer = RegInit(VecInit(Seq.fill(VREG_DEPTH/NUM_MEMORY_BANKS)(VecInit(Seq.fill(NUM_MEMORY_BANKS)(0.S(FIXED_WIDTH.W))))))
  /** Write-enable signal to register file */
  val we = RegInit(false.B)

  // --- LOGIC ---
  //Lookup table mapping current iteration to the indices of face neighbours
  val fcn = Array.ofDim[Vec[UInt]](8)
  fcn(0) = VecInit(Seq(1.U, 2.U, 4.U))
  fcn(1) = VecInit(Seq(0.U, 3.U, 5.U))
  fcn(2) = VecInit(Seq(0.U, 3.U, 6.U))
  fcn(3) = VecInit(Seq(1.U, 2.U, 7.U))
  fcn(4) = VecInit(Seq(0.U, 5.U, 6.U))
  fcn(5) = VecInit(Seq(1.U, 4.U, 7.U))
  fcn(6) = VecInit(Seq(2.U, 4.U, 7.U))
  fcn(7) = VecInit(Seq(3.U, 5.U, 6.U))
  val fcnLookup = VecInit(fcn)

  //Lookup table mapping current iteration to the indices of edge neighbours
  val edn = Array.ofDim[Vec[UInt]](8)
  edn(0) = VecInit(Seq(3.U, 5.U, 6.U))
  edn(1) = VecInit(Seq(2.U, 4.U, 7.U))
  edn(2) = VecInit(Seq(1.U, 4.U, 7.U))
  edn(3) = VecInit(Seq(0.U, 5.U, 6.U))
  edn(4) = VecInit(Seq(1.U, 2.U, 7.U))
  edn(5) = VecInit(Seq(0.U, 3.U, 6.U))
  edn(6) = VecInit(Seq(0.U, 3.U, 5.U))
  edn(7) = VecInit(Seq(1.U, 2.U, 4.U))
  val ednLookup = VecInit(edn)

  //Next state logic
  switch(state) {
    is(sIdleOutput) {
      when(valid && (mod === VEC || mod === DOF)) {
        state := sVec
        cnt := cnt + 1.U
      } .elsewhen(valid && mod === ELEM) {
        state := sElem
        cnt := cnt + 1.U
      } .elsewhen(valid && mod === SEL) {
        we := true.B //No state necessary
      } .elsewhen(valid && mod === FCN) {
        state := sFcn
        cnt := cnt + 3.U
      } .elsewhen(valid && (mod === EDN1 || mod === EDN2)) { //EDN1 and EDN2 map to same indices
        state := sEdn
        cnt := cnt + 3.U
      }
      when(we && mod =/= SEL) { //SEL requires no building, must keep we high
        we := false.B
      } .elsewhen(!valid) { //Once valid is deasserted, untoggle we
        we := false.B
      }
    }
    is(sVec) {
      when(valid) {
        cnt := cnt + 1.U
      }
      when(valid && cnt === (VREG_DEPTH / NUM_MEMORY_BANKS - 1).U) {
        state := sIdleOutput
        cnt := 0.U
        we := true.B
      }
    }
    is(sElem) {
      when(valid) {
        cnt := cnt + 1.U
      }
      when(valid && cnt === (XREG_DEPTH - 1).U) {
        state := sIdleOutput
        cnt := 0.U
        we := true.B
      }
    }
    is(sFcn) {
      when(valid && cnt === 3.U) {
        state := sIdleOutput
        cnt := 0.U
        we := true.B
      }
    }
    is(sEdn) {
      when(valid && cnt === 3.U) {
        state := sIdleOutput
        cnt := 0.U
        we := true.B
      }
    }
  }

  //Write buffer logic
  switch(mod) {
    is(DOF) {
      when(valid) {
        writeBuffer(cnt) := rdData
      }
    }
    is(VEC) {
      when(valid) {
        writeBuffer(cnt) :=rdData
      }
    }
    is(ELEM) {
      when(valid) {
        writeBuffer(0)(cnt) := rdData(iteration) //iteration number defines which of the read data is valid
      }
    }
    is(SEL) {
      when(valid) {
        writeBuffer(0)(cnt) := rdData(iteration)
        for(i <- 1 until XREG_DEPTH) {
          val wb = i/NUM_MEMORY_BANKS
          val j = i % NUM_MEMORY_BANKS
          writeBuffer(wb)(j) := 0.S
        }
      }
    }
    is(FCN) {
      when(valid) {
        writeBuffer(0)(cnt) := rdData(fcnLookup(iteration)(0))
        writeBuffer(0)(cnt+1.U) := rdData(fcnLookup(iteration)(1))
        writeBuffer(0)(cnt+2.U) := rdData(fcnLookup(iteration)(2))
      }
    }
    is(EDN1) {
      when(valid) {
        writeBuffer(0)(cnt) := rdData(ednLookup(iteration)(0))
        writeBuffer(0)(cnt+1.U) := rdData(ednLookup(iteration)(1))
        writeBuffer(0)(cnt+2.U) := rdData(ednLookup(iteration)(2))
      }
    }
    is(EDN2) {
      when(valid) {
        writeBuffer(0)(cnt) := rdData(ednLookup(iteration)(0))
        writeBuffer(0)(cnt+1.U) := rdData(ednLookup(iteration)(1))
        writeBuffer(0)(cnt+2.U) := rdData(ednLookup(iteration)(2))
      }
    }
  }

  //Output connections
  for(i <- 0 until VREG_DEPTH) {
    val wb = i/NUM_MEMORY_BANKS
    val j = i % NUM_MEMORY_BANKS
    io.id.wrData(i) := writeBuffer(wb)(j)
  }
  io.mem.ready := true.B //Don't need the ready signalling, so this is tied high
  io.readQueue.ready := valid
  io.id.we := we
  io.id.rd := rd
}

/**
 * The possible states that [[MemoryWriteback]] may take on. These are used to control how writeback results are generated.
 */
object MemoryWritebackState extends ChiselEnum {
  val sIdleOutput, sVec, sElem, sFcn, sEdn, sEdn1_2, sEdn2_1, sEdn2_2 = Value
}

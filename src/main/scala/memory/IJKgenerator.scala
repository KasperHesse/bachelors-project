package memory

import chisel3._
import chisel3.util._
import utils.Config._

/**
 * I/O ports for an [[IJKgenerator]]
 */
class IJKgeneratorIO extends Bundle {
  /** Outputs going to memory stage and other thread */
  val out = Output(new IJKgeneratorBundle)
  /** Input values from other thread */
  val in = Flipped(new IJKgeneratorBundle)
  /** Control signals from [[execution.IJKGeneratorFSM]] */
  val ctrl = new IJKgeneratorControl
}

/**
 * A bundle holding the output values generated by an [[IJKgenerator]].
 * Use as-is on the output port of an IJK generator, use Flipped() on the input port of the generator and consumer modules
 */
class IJKgeneratorBundle extends Bundle {
  /** Current i,j,k values */
  val ijk = Output(new IJKBundle)
  /** Current iteration number. If >=8, the output is invalid */
  val iteration = Output(UInt(4.W))
  /** Asserted when the ijk and iteration values may be sampled by the other thread */
  val valid = Output(Bool())
}

/**
 * A bundle holding the control signals input to an [[IJKgenerator]] by a [[execution.IJKGeneratorFSM]]
 */
class IJKgeneratorControl extends Bundle {
  /** Restart signal. Makes the generator restart iterating from its saved i,j,k,iteration values */
  val restart = Input(Bool())
  /** Load signal. When asserted, loads in the i,j,k,iteration values from another IJK generator */
  val load = Input(Bool())
  /** When asserted, causes the output to be updated on the next clock cycle with the next i,j,k value to be generated */
  val next = Input(Bool())
  /** Whether the output IJK bundle should be considered padding or not. Asserted when out.iteration >= 8 */
  val pad = Output(Bool())
}

/**
 * The module used to generate i,j,k indices. Implements [[IJKgeneratorIO]]
 */
class IJKgenerator extends Module {
  val io = IO(new IJKgeneratorIO)

  // --- REGISTERS
  /** Iteration / colouring value */
  val iteration = RegInit(0.U(4.W))

  val iterationNext = WireDefault(0.U(4.W))
  /** Saved iteration value, used when restarting */
  val iterationSaved = RegInit(0.U(4.W))
  /** Whether the output is valid or not. When asserted, all i,j,k-values have been iterated through */
  val invalidFlag = WireDefault(false.B)
  /** Sequence of starting values, used to initialize [[startVec]] */
  val startValues = Seq(Seq(0,0,0), Seq(0,1,0), Seq(0,0,1), Seq(0,1,1), Seq(1,0,0), Seq(1,1,0), Seq(1,0,1), Seq(1,1,1))
  /** Vector of starting values, used to set starting offsets when starting a new iteration / colouring */
  val startVec = Wire(Vec(8, Vec(3, UInt(log2Ceil(GDIM).W))))
  for(i <- startVec.indices) {
    startVec(i) := VecInit(startValues(i).map(_.U))
  }
  val i = RegInit(0.U(log2Ceil(GDIM+3).W))
  val j = RegInit(0.U(log2Ceil(GDIM+3).W))
  val k = RegInit(0.U(log2Ceil(GDIM+3).W))

  val iSaved = RegInit(0.U(log2Ceil(GDIM+3).W))
  val jSaved = RegInit(0.U(log2Ceil(GDIM+3).W))
  val kSaved = RegInit(0.U(log2Ceil(GDIM+3).W))

  // --- SIGNALS ---
  val jUpdate: Bool = j + 2.U >= NELY.U
  val kUpdate: Bool = k + 2.U >= NELZ.U
  val iUpdate: Bool = i + 2.U >= NELX.U
  val iNext = WireDefault(0.U)
  val kNext = WireDefault(0.U)
  val jNext = WireDefault(0.U)
  iNext := i
  kNext := k
  jNext := Mux(invalidFlag, j, j + 2.U) //Once iter = 8, don't update j and other values
  iterationNext := iteration

  when(jUpdate) {
    jNext := startVec(iteration)(1)
    kNext := k + 2.U
  }
  when(kUpdate && jUpdate) {
    kNext := startVec(iteration)(2)
    iNext := i + 2.U
  }
  when(iUpdate && kUpdate && jUpdate) {
    iNext := startVec(iteration + 1.U)(0)
    jNext := startVec(iteration + 1.U)(1)
    kNext := startVec(iteration + 1.U)(2)
    iterationNext := iteration + 1.U
  }
  invalidFlag := iteration >= 8.U


  when(io.ctrl.restart) {
    i := iSaved
    j := jSaved
    k := kSaved
    iteration := iterationSaved
  } .elsewhen(io.ctrl.load) {
    i := io.in.ijk.i
    j := io.in.ijk.j
    k := io.in.ijk.k
    iteration := io.in.iteration
    iSaved := io.in.ijk.i
    jSaved := io.in.ijk.j
    kSaved := io.in.ijk.k
    iterationSaved := io.in.iteration
  }. elsewhen(io.ctrl.next) {
    i := iNext
    j := jNext
    k := kNext
    iteration := iterationNext
  }

  // --- OUTPUTS ---
  io.out.ijk.i := i
  io.out.ijk.j := j
  io.out.ijk.k := k
  io.out.iteration := iteration
  io.ctrl.pad := invalidFlag

  io.out.valid := DontCare //Not set in this module
}

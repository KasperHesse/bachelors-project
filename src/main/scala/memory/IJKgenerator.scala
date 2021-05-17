package memory

import chisel3._
import chisel3.util._
import utils.Config._

class IJKgeneratorIO extends Bundle {
  val in = Input(new IJK)
  val out = Output(new IJK)
  val load = Input(Bool())
  val restart = Input(Bool())
  val valid = Output(Bool())
  val ready = Input(Bool())
}

class IJK extends Bundle {
  val i = UInt(log2Ceil(GDIM+3).W)
  val j = UInt(log2Ceil(GDIM+3).W)
  val k = UInt(log2Ceil(GDIM+3).W)
  val iteration = UInt(4.W)
}

class IJKgenerator extends Module {
  val io = IO(new IJKgeneratorIO)

  // --- REGISTERS
  /** Iteration / colouring value */
  val iteration = RegInit(0.U(4.W))

  val iterationSaved = RegInit(0.U(4.W))
  /** Whether the output is valid or not. When asserted, all i,j,k-values have been iterated through */
  val invalidFlag = WireDefault(false.B)
  /** Sequence of starting values, used to initialize [[startVec]] */
  val startValues = Seq(Seq(0,0,0), Seq(0,0,1), Seq(0,1,0), Seq(0,1,1), Seq(1,0,0), Seq(1,0,1), Seq(1,1,0), Seq(1,1,1))
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
    iteration := iteration + 1.U
  }
  invalidFlag := iteration >= 8.U


  when(io.restart) {
    i := iSaved
    j := jSaved
    k := kSaved
    iteration := iterationSaved
  } .elsewhen(io.load) {
    i := io.in.i
    j := io.in.j
    k := io.in.k
    iteration := io.in.iteration
    iSaved := io.in.i
    jSaved := io.in.j
    kSaved := io.in.k
    iterationSaved := io.in.iteration
  }. elsewhen(io.ready) {
    i := iNext
    j := jNext
    k := kNext
  }

  // --- OUTPUTS ---
  io.out.i := i
  io.out.j := j
  io.out.k := k
  io.out.iteration := iteration
  io.valid := !invalidFlag
}

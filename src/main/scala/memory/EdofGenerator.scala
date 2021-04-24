package memory

import chisel3._
import chisel3.util._

/**
 * Generates the 24 element DOF-indices, used for accessing node DOF's in vectors.
 * Latches in the values when they are supplied and valid is asserted. Outputs the values in three consecutive clock cycles,
 * each with valid asserted on the output, once computation is finished. Implements [[EdofGeneratorIO]].
 * @param nx Number of nodes in the x-direction of the grid (nelx + 1)
 * @param ny Number of nodes in the y-direction of the grid (nely + 1)
 * @param nz Number of nodes in the z-direction of the grid (nelz + 1)
 * @param width The number of values to output on each clock cycle
 */
class EdofGenerator(val nx: Int, val ny: Int, val nz: Int, val width: Int) extends Module {
  val ndof: Int = 3*ny*nx*nz
  val io: EdofGeneratorIO = IO(new EdofGeneratorIO(ndof))

  val outputStep = RegInit(0.U(2.W)) //Which part-of-8 of the indices is currently being output?

  val nIndex = Wire(Vec(8, UInt(log2Ceil(ndof).W)))

  //Only latch in values if we're not currently processing, or if we're ready to process next segment
  val validReg = RegInit(false.B)
  val ready = (outputStep === 0.U && !validReg) || outputStep === 2.U
  val nIndexReg = RegEnable(nIndex, ready)

  val in = RegEnable(io.in, ready)

  val i = in.i
  val j = in.j
  val k = in.k

  //Handle initial multiplications. Currently implemented with a lookup table
  val nynzLookup: Seq[Vec[UInt]] = for (i <- 0 until 2) yield {
    Wire(Vec(nx, UInt(log2Ceil(ndof).W)))
  }
  val nyLookup: Seq[Vec[UInt]] = for(i <- 0 until 2) yield {
    Wire(Vec(ny, UInt(log2Ceil(ndof).W)))
  }

  //Fill up lookup tables
  for(i <- 0 until nx) {
    nynzLookup(0)(i) := (i * ny*nz).U
    nynzLookup(1)(i) := (i * ny*nz).U
  }
  for(i <- 0 until ny) {
    nyLookup(0)(i) := (i*ny).U
    nyLookup(1)(i) := (i*ny).U
  }

  //See top.c, getEdof for calculations that spawned this
  val nx1 = i
  val nx2 = i + 1.U
  val ny1 = j
  val ny2 = j + 1.U
  val nz1 = k
  val nz2 = k + 1.U

  val nynz_p1: UInt = nynzLookup(0)(nx1)
  val nynz_p2: UInt = nynzLookup(1)(nx2)

  val ny_p1: UInt = nyLookup(0)(nz1)
  val ny_p2: UInt = nyLookup(1)(nz2)

  nIndex(0) := nynz_p1 + ny_p1 + ny2
  nIndex(1) := nynz_p2 + ny_p1 + ny2
  nIndex(2) := nynz_p2 + ny_p1 + ny1
  nIndex(3) := nynz_p1 + ny_p1 + ny1
  nIndex(4) := nynz_p1 + ny_p2 + ny2
  nIndex(5) := nynz_p2 + ny_p2 + ny2
  nIndex(6) := nynz_p2 + ny_p2 + ny1
  nIndex(7) := nynz_p1 + ny_p2 + ny1

  //Output stage
  validReg := Mux(ready, in.valid, validReg)
  outputStep := Mux(validReg, Mux(outputStep === 2.U, 0.U, outputStep + 1.U), outputStep)

  for(i <- 0 until 8) {
    io.out.offsets(i) := (nIndexReg(i) << 1.U).asUInt() + nIndexReg(i) + outputStep
  }
  io.out.valid := validReg
  io.out.ready := ready



}

/**
 * I/O ports for [[EdofGenerator]]
 * @param ndof The total number of element DOF's in the design
 */
class EdofGeneratorIO(val ndof: Int) extends Bundle {
  val in = Input(new EdofGeneratorInput(ndof))
  val out = Output(new EdofGeneratorOutput(ndof))

  /**
   * Input ports for element DOF generator
   * @param ndof The total number of element DOF's in the design
   */
  class EdofGeneratorInput(val ndof: Int) extends Bundle {
    /** Element index in the x-direction */
    val i = UInt(log2Ceil(ndof).W)
    /** Element index in the y-direction */
    val j = UInt(log2Ceil(ndof).W)
    /** Element index in the z-direction */
    val k = UInt(log2Ceil(ndof).W)
    /** Indicates that inputs i,j,k are valid */
    val valid = Bool()
  }

  /**
   * Output ports for element DOF generator
   * @param ndof The total number of element DOF's in the design
   */
  class EdofGeneratorOutput(val ndof: Int) extends Bundle {
    /** A number of address offsets, used to select the correct memory locations */
    val offsets = Vec(8, UInt(log2Ceil(ndof).W))
    /** Indicates that output offset values are valid */
    val valid = Bool()
    /** If high, a new set of i,j,k values will be latched in when next 'valid' is asserted */
    val ready = Bool()
  }



}

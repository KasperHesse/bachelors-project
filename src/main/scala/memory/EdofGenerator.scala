package memory

import chisel3._
import chisel3.util._
import utils.Config._


/**
 * Interface between an IJK generator inside a Thread and the EDOF generator located before the memory stage
 * Instantiate as-is in the IJK-generator/thread, use Flipped() in the edof generator
 */
class IJKgeneratorEdofIO extends Bundle {
  /** Element index in the x-direction */
  val i = UInt(log2Ceil(NDOF+1).W)
  /** Element index in the y-direction */
  val j = UInt(log2Ceil(NDOF+1).W)
  /** Element index in the z-direction */
  val k = UInt(log2Ceil(NDOF+1).W)
  /** Flag transmitted through to memory stage. */
  val pad = Bool()
}

/**
 * Interface between the EDOF generator and memory stage.
 * Instntiate as-is in the EDOF generator, use Flipped() in the memory stage
 */
class EdofMemoryIO extends Bundle {
  /** The indices used as offsets from the base memory address */
  val indices = Vec(8, UInt(log2Ceil(NDOF+1).W))
  /** Flag transmitted through to memory stage */
  val pad = Bool()
}

/**
 * I/O ports for [[EdofGenerator]]
 */
class EdofGeneratorIO extends Bundle {
  val prod = Flipped(Decoupled(new IJKgeneratorEdofIO))
  val cons = Decoupled(new EdofMemoryIO)
}

/**
 * Generates the 24 element DOF-indices, used for accessing node DOF's in vectors.
 * Latches in the values when the ready/valid handshake is performed.
 * Outputs the indices in 3 rounds of 8 values, outputting the next bundle whenever the consumer signals 'ready'.
 * When the final round is output, it is immediatedly ready to receive new ijk-values on the input. Implements [[EdofGeneratorIO]].
 */
class EdofGenerator extends Module {
  val io: EdofGeneratorIO = IO(new EdofGeneratorIO)

  //--- REGISTERS ---
  /** Which section of indices is currently being output */
  val outputStep = RegInit(0.U(2.W)) //Which part-of-8 of the indices is currently being output?
  /** Flag indicating whether the generator is currently processing or not. Also used as output valid bit */
  val processing = RegInit(false.B)
  /** Vector holding index output values */
  val nIndex = Wire(Vec(8, UInt(log2Ceil(NDOF+1).W)))
  /** Asserted when this module is ready to receive new data */
  val readyInternal = WireDefault(false.B)
  /** Asserted when the output from this module is valid */
  /** Pipeline register holding input values */
  val in = RegEnable(io.prod.bits, io.prod.valid && readyInternal)


  // --- SIGNALS AND WIRES ---
  //Shorthand acceeses
  val i = in.i
  val j = in.j
  val k = in.k

  //Handle initial multiplications. Implemented with a LUT to make our life easier. Given x,y-value, we output x*NY*NZ and y*NY
  val nynzLookup: Seq[Vec[UInt]] = for (i <- 0 until 2) yield {
    Wire(Vec(NX, UInt(log2Ceil(NDOF+1).W)))
  }
  val nyLookup: Seq[Vec[UInt]] = for(i <- 0 until 2) yield {
    Wire(Vec(NY, UInt(log2Ceil(NDOF+1).W)))
  }

  //Fill up lookup tables
  for(i <- 0 until NX) {
    nynzLookup(0)(i) := (i * NY*NZ).U
    nynzLookup(1)(i) := (i * NY*NZ).U
  }
  for(i <- 0 until NY) {
    nyLookup(0)(i) := (i*NY).U
    nyLookup(1)(i) := (i*NY).U
  }

  //See top.c, getEdof for calculations that spawned this monstrosity
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

  // -- CONNECTIONS AND UPDATE LOGIC
  //ready is entirely combinational logic
  readyInternal := (outputStep === 0.U && !processing) || (outputStep === 2.U && io.cons.ready)

  when(readyInternal && io.prod.valid) {
    processing := true.B
  } .elsewhen(outputStep === 2.U && io.cons.ready) {
    processing := false.B
  }
  outputStep := Mux(processing && io.cons.ready, Mux(outputStep === 2.U, 0.U, outputStep + 1.U), outputStep)

  for(i <- 0 until 8) {
    //3*nIndex(i) + (0,1,2)
    io.cons.bits.indices(i) := (nIndex(i) << 1.U).asUInt() + nIndex(i) + outputStep
  }
  io.cons.valid := processing
  io.prod.ready := readyInternal
  io.cons.bits.pad := in.pad
}


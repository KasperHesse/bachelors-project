package memory

import chisel3._
import chisel3.util._
import utils.Config._

class IndexGeneratorIO extends Bundle {
  val in = Flipped(Decoupled(new NeighbourGenIndexGenIO))
  val addrGen = Decoupled(new AddressGenProducerIO)
}

/**
 * This module takes 3 i,j,k-tuples and converts them into the global element offset in a vector.
 * If the given tuple does not represent a valid position in the given vector, the 'validIndices' flag is toggled low,
 * but the output values are *not* changed. Implements [[IndexGeneratorIO]]
 *
 * @param pipe Whether to insert a pipeline register at the input of this module (true) or not (false)
 *
 */
class IndexGenerator(val pipe: Boolean = true) extends Module {
  val io = IO(new IndexGeneratorIO)
  /** Number of input ports. Is 3, since at most 3 indices can be generated in one cycle by the [[NeighbourGenerator]] */
  val NUM_INPUT_PORTS = 3

  //if no data has been received, output is invalid and input is ready
  //When data has been received, output is valid and input is not ready
  //When consumer is ready, we are also ready
  //If no valid data was input when consumer was ready, we are no longer valid

  // --- REGISTERS ---
  /** Ready signal to producer */
  val readyInternal = WireDefault(true.B)
  /** Valid signal to consumer */
  val validInternal = if(pipe) RegInit(false.B) else WireDefault(false.B)
  val in = if(pipe) RegEnable(io.in.bits, io.in.valid && readyInternal) else io.in.bits


  val NELXH = (NELX+1)/2
  val NELYH = (NELY+1)/2
  val NELZH = (NELZ+1)/2
  /** Lookup table for computing i/2*NELYH*NELZH */
  val nxLookup = Wire(Vec(NUM_INPUT_PORTS, Vec(NELXH, UInt(log2Ceil(NDOF+1).W))))
  
  /** Lookup table for computing k/2*NELYH */
  val nzLookup = Wire(Vec(NUM_INPUT_PORTS, Vec(NELZH, UInt(log2Ceil(NDOF+1).W))))

  for(nx <- 0 until NELXH) {
    nxLookup.foreach(vec => vec(nx) := (nx*NELYH*NELZH).U)
  }
  for(nz <- 0 until NELZH) {
    nzLookup.foreach(vec => vec(nz) := (nz*NELYH).U)
  }

  val nx = in.ijk.map(ijk => (ijk.i >> 1).asUInt())
  val ny = in.ijk.map(ijk => (ijk.j >> 1).asUInt())
  val nz = in.ijk.map(ijk => (ijk.k >> 1).asUInt())

  /** Vector holding calculated indices */
  val indices = VecInit(Seq.fill(NUM_MEMORY_BANKS)(0.U(log2Ceil(NDOFLENGTH+1).W)))
  /** Vector holding valid flags for each calculated index */
  val validIndices = VecInit(Seq.fill(NUM_MEMORY_BANKS)(false.B))

  // --- LOGIC ---
  //Calculate all indices and set valid flags
  for(x <- 0 until NUM_INPUT_PORTS) {
    val row = nxLookup(x)(nx(x)) + nzLookup(x)(nz(x)) + ny(x)

    val a = (in.ijk(x).i(0) << 2).asUInt()
    val b = (in.ijk(x).k(0) << 1).asUInt()
    val c = in.ijk(x).j(0)
    val bank = a | b | c
    //Rows signify which element block is being accessed
    //Banks indicate which element in a memory row should be indexed. IJK iteration is reconstructed from LSB of i,j and k
    indices(x) := Cat(row, bank)
    validIndices(x) := Mux(in.ijk(x).i < NELX.U & in.ijk(x).j < NELY.U & in.ijk(x).k < NELZ.U, in.validIjk(x), false.B)
  }

  //ready-valid signal handling
  if(pipe) {
    when(!validInternal && io.in.valid && readyInternal) {
      validInternal := true.B
    } .elsewhen(validInternal & io.addrGen.ready & !io.in.valid) {
      validInternal := false.B
    }
    when(!validInternal) {
      readyInternal := true.B
    } .otherwise {
      readyInternal := io.addrGen.ready
    }
  } else {
    validInternal := io.in.valid
    readyInternal := io.addrGen.ready
  }

  //Outputs
  io.addrGen.bits.indices := indices
  io.addrGen.bits.validIndices := validIndices
  io.addrGen.bits.baseAddr := in.baseAddr

  io.in.ready := readyInternal
  io.addrGen.valid := validInternal
}

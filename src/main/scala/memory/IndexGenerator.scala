package memory

import chisel3._
import chisel3.util._
import utils.Config._

class IndexGeneratorIO extends Bundle {
  val in = Flipped(Decoupled(new IndexGeneratorProducerIO))
  val addrGen = Decoupled(new AddressGenProducerIO)
}

/**
 * This module takes a number of i,j,k-tuples and converts them into the global offset in a vector.
 * If the given tuple does not represent a valid position in the given vector, the 'validIndices' flag is toggled low,
 * but the output values are *not* changed. Implements [[IndexGeneratorIO]]
 *
 * @param pipe Whether to insert a pipeline register at the input of this module (true) or not (false)
 *
 */
class IndexGenerator(val pipe: Boolean = true) extends Module {
  val io = IO(new IndexGeneratorIO)

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


  // --- SIGNALS AND WIRES ---
  /** Lookup tables for computing i*nely*nelz */
  val nelxnelyLookup = for(i <- 0 until NUM_MEMORY_BANKS) yield {
    Wire(Vec(NELX, UInt((log2Ceil(NDOF)+1).W))) //Declared with one additional bit so we can perform arithmetic comparisons
  }
  /** Lookup tables for computing k*nely */
  val nelyLookup = for(i <- 0 until NUM_MEMORY_BANKS) yield {
    Wire(Vec(NELY, UInt((log2Ceil(NDOF)+1).W)))
  }

  //Fill lookup tables
  for(i <- nelxnelyLookup.indices) {
    for(j <- 0 until NELX) {
      nelxnelyLookup(i)(j) := (j * NELY * NELZ).U
    }
  }
  for(i <- nelyLookup.indices) {
    for(j <- 0 until NELY) {
      nelyLookup(i)(j) := (j * NELY).U
    }
  }
  /** Vector holding calculated indices */
  val indices = Wire(Vec(NUM_MEMORY_BANKS, UInt(log2Ceil(NDOF+1).W)))
  /** Vector holding valid flags for each calculated index */
  val validIndices = Wire(Vec(NUM_MEMORY_BANKS, Bool()))

  // --- LOGIC ---
  //Calculate all indices and set valid flags
  for(i <- 0 until NUM_MEMORY_BANKS) {
    //i * nely * nelz + k * nely + j
    indices(i) := nelxnelyLookup(i)(in.ijk(i).i) + nelyLookup(i)(in.ijk(i).k) + in.ijk(i).j
    validIndices(i) := Mux(in.ijk(i).i < NELX.U & in.ijk(i).j < NELY.U & in.ijk(i).k < NELZ.U, in.validIjk(i), false.B)
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
  io.addrGen.bits.ls := in.ls

  io.in.ready := readyInternal
  io.addrGen.valid := validInternal

}

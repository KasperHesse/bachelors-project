package memory

import chisel3._
import chisel3.util._
import execution.StypeMod
import utils.Config._

/**
 * I/O ports for [[EdofGenerator]]
 */
class EdofGeneratorIO extends Bundle {
  /** Input ports from IJK generator */
  val in = Flipped(Decoupled(new IJKgeneratorConsumerIO))
  /** Outputs to address generator */
  val addrGen = Decoupled(new AddressGenProducerIO)
}

class EdofWrapper extends Module {
  val io = IO(new Bundle {
    val in = Flipped(Decoupled(new IJKgeneratorConsumerIO))
    val out = Decoupled(new IJKgeneratorConsumerIO)
  })
  val reg = RegEnable(io.in.bits, io.out.ready)
  val valid = RegEnable(io.in.valid, io.out.ready)
  io.out.valid := valid
  io.out.bits := reg
  io.in.ready := io.out.ready

}

/**
 * Generates the 24 element DOF-indices, used for accessing node DOF's in vectors.
 * Latches in the values when the ready/valid handshake is performed.
 * Outputs the indices in 3 rounds of 8 values, outputting the next bundle whenever the consumer signals 'ready'.
 * When the final round is output, it is immediatedly ready to receive new ijk-values on the input. Implements [[EdofGeneratorIO]].
 */
class EdofGenerator extends Module {
  val io: EdofGeneratorIO = IO(new EdofGeneratorIO)

  // --- CONSTANTS ---
  //xxH corresponds to math.ceil(xx/2). xxL corresponds to math.floor(xx/2)
  val NXH = (NX + 1)/2
  val NXL = NX/2
  val NYH = (NY + 1)/2
  val NYL = NY/2
  val NZH = (NZ+1)/2
  val NZL = NZ/2

  //--- REGISTERS ---
  /** Which section of indices is currently being output */
  val outputStep = RegInit(0.U(2.W))
  /** Flag indicating whether the generator is currently processing or not. Also used as output valid bit */
  val processing = RegInit(false.B)
  /** Vector holding index output values */
  val nIndex = Wire(Vec(8, UInt(log2Ceil(NDOF+1).W)))
  /** Asserted when this module is ready to receive new data */
  val readyInternal = WireDefault(false.B)
  /** Pipeline register holding input values */
  val in = RegEnable(io.in.bits, io.in.valid && readyInternal)
  /** Vector holding the indices being output to address generator */
  val indices = Wire(Vec(NUM_MEMORY_BANKS, UInt(log2Ceil(NDOF+1).W)))
  /** Output registers holding the values to addr. generator */
  val indicesReg = RegInit(VecInit(Seq.fill(NUM_MEMORY_BANKS)(0.U(log2Ceil(NDOF+1).W))))
  /** vector holding the valid flags for output indices */
  val validIndices = Wire(Vec(NUM_MEMORY_BANKS, Bool()))

  // --- MODULES ---
  val nz0Lookup = Module(new NzLookup(NZL))
  val nz1Lookup = Module(new NzLookup(NZH))

  val nx0lookup = Module(new NxLookup(NXL))
  val nx1lookup = Module(new NxLookup(NXH))


  // --- SIGNALS AND WIRES ---
  //Shorthand acceeses
  val i = in.ijk.i
  val j = in.ijk.j
  val k = in.ijk.k

  val nx0 = (i >> 1).asUInt
  val nx1 = (i >> 1).asUInt + i(0) //adding i(0) corresponds math.ceil(i/2)
  val ny0 = (j >> 1).asUInt
  val ny1 = (j >> 1).asUInt + j(0)
  val nz0 = (k >> 1).asUInt
  val nz1 = (k >> 1).asUInt + k(0)

  nx0lookup.io.in := nx0
  nx1lookup.io.in := nx1
  nz1Lookup.io.in := nz1
  nz0Lookup.io.in := nz0

  nIndex(0) := nx1lookup.io.nyhnzh + nz1Lookup.io.nyh + ny1
  nIndex(1) := nx1lookup.io.nylnzh + nz1Lookup.io.nyl + ny0
  nIndex(2) := nx1lookup.io.nyhnzl + nz0Lookup.io.nyh + ny1
  nIndex(3) := nx1lookup.io.nylnzl + nz0Lookup.io.nyl + ny0
  nIndex(4) := nx0lookup.io.nyhnzh + nz1Lookup.io.nyh + ny1
  nIndex(5) := nx0lookup.io.nylnzh + nz1Lookup.io.nyl + ny0
  nIndex(6) := nx0lookup.io.nyhnzl + nz0Lookup.io.nyh + ny1
  nIndex(7) := nx0lookup.io.nylnzl + nz0Lookup.io.nyl + ny0

  // -- CONNECTIONS AND UPDATE LOGIC
  readyInternal := (outputStep === 0.U && !processing) || (outputStep === 2.U && io.addrGen.ready)
  when(readyInternal && io.in.valid) {
    processing := true.B
  } .elsewhen(outputStep === 2.U && io.addrGen.ready) {
    processing := false.B
  }

  when(processing && outputStep === 0.U) {
    outputStep := outputStep + 1.U
    indicesReg := indices
  } .elsewhen(processing && io.addrGen.ready && outputStep === 1.U) {
    outputStep := outputStep + 1.U
    indicesReg := indices
  } .elsewhen(processing && io.addrGen.ready && outputStep === 2.U) {
    outputStep := 0.U
    indicesReg := indices
  } .otherwise {
    outputStep := outputStep
  }

  for(i <- 0 until 8) {
    //24*nIndex(i) + outputStep*8 + i
    //Performed as (nIndex(i)*3 + outputStep)*8, where *8 is performed by a later bitshift
    //That bitshift is performed by concatenating the i value into the lower 3 bits
    val indexX3 = (nIndex(i) << 1).asUInt + nIndex(i)
    val offset = i.U(3.W)
    val out = Cat(indexX3 + outputStep, offset) //Defined as separate value to make VCD outputs nicer
    indices(i) := out
  }

  when(in.mod === StypeMod.DOF) {
    for(i <- 0 until 8) {
      validIndices(i) := !in.pad
    }
  } .otherwise { //Fixed DOF. Only the lower 4 outputs may be valid, only the case when nx1=0 (lower DOFs of layer 0)
    for(i <- 0 until 4) {
      validIndices(i) := nx1 === 0.U
    }
    for(i <- 4 until 8) {
      validIndices(i) := false.B
    }
  }

  //Must delay the output by one clock cycle to ensure that write data has arrived in write queue and
  //this module is connected to address generator
  io.addrGen.bits.validIndices := RegNext(validIndices)
  io.addrGen.bits.indices := indicesReg
  io.addrGen.bits.baseAddr := RegNext(in.baseAddr)
  io.addrGen.valid := RegNext(processing)
  io.in.ready := readyInternal
}

/**
 * A module implementing a lookup table for calculating {@code nz*NYH} and {@code nz*NYL}
 * @param entries The number of entries in the lookup table. If {@code nz0} is to be used as input, use {@code NZL}.
 *                For `nz1` as input, use {@code NZH}
 */
class NzLookup(entries: Int) extends Module {
  val NYL = NY/2
  val NYH = (NY+1)/2

  val io = IO(new Bundle {
    /** The input value to be handled in the LUT */
    val in = Input(UInt(log2Ceil(GDIM+1).W))
    /** The result of calculating in * NYH */
    val nyh = Output(UInt(log2Ceil(NDOF+1).W))
    /** The result of calculating in * NYL */
    val nyl = Output(UInt(log2Ceil(NDOF+1).W))
  })

  val lookup = Wire(Vec(entries, Vec(2, UInt(log2Ceil(NDOF+1).W))))
  for(nz <- 0 until entries) {
    lookup(nz)(0) := (nz * NYH).U
    lookup(nz)(1) := (nz * NYL).U
  }
  io.nyh := lookup(io.in)(0)
  io.nyl := lookup(io.in)(1)
}
/**
 * A module implementing a lookup table for calculating {@code nx*NYH*NZH, nx*NYH*NZL, nx*NYL*NZH},  {@code nx*NYL*NZL}
 * @param entries The number of entries in the lookup table. If {@code nx0} is to be used as input, use {@code NXL}.
 *                For {@code nx1} as input, use {@code NXH}
 */
class NxLookup(entries: Int) extends Module {
  val NYL = NY/2
  val NZL = NZ/2
  val NYH = (NY+1)/2
  val NZH = (NZ+1)/2
  val io = IO(new Bundle {
    /** The input value to be mapped in the lookup table */
    val in = Input(UInt(log2Ceil(GDIM+1).W))
    /** The result of computing in * NYH * NZH */
    val nyhnzh = Output(UInt(log2Ceil(NDOF+1).W))
    /** The result of computing in*NYH*NZL */
    val nyhnzl = Output(UInt(log2Ceil(NDOF+1).W))
    /** The result of computing in*NYL*NZH */
    val nylnzh = Output(UInt(log2Ceil(NDOF+1).W))
    /** The result of computing in*NYL*NZL */
    val nylnzl = Output(UInt(log2Ceil(NDOF+1).W))
  })

  val lookup = Wire(Vec(entries, Vec(4, UInt(log2Ceil(NDOF+1).W))))
  for(i <- 0 until entries) {
    lookup(i)(0) := (i * NYH * NZH).U
    lookup(i)(1) := (i * NYH * NZL).U
    lookup(i)(2) := (i * NYL * NZH).U
    lookup(i)(3) := (i * NYL * NZL).U
  }

  io.nyhnzh := lookup(io.in)(0)
  io.nyhnzl := lookup(io.in)(1)
  io.nylnzh := lookup(io.in)(2)
  io.nylnzl := lookup(io.in)(3)
}
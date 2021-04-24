package vector

import chisel3._
import chisel3.util._
import chisel3.experimental.ChiselEnum
import utils.Fixed.FIXED_WIDTH
import utils.Config._
import vector.Opcode._
/*
Vi skal holde styr på hvor langt vi er i den nuværende vektor og operation.
Problem: Jeg er ikke konsekvent i hvad der er vektoren (er den 32-lang eller variabel længde?) og operationen (samme problem)

Vi skal tage imod en ny *vektor* fra register file, når vi er færdige med at processere den nuværende
- vectorLength er antal elementer i den nuværende vektor som skal processeres
- - Hvis operationen er ADD, SUB, MUL, DIV lægger vi ny adresse ind hver gang
- - Hvis operationen er DOT, MVP, SUM lægger vi kun adressen ind idet elementsProcessed == 0
Vi skal smide en ny destination i address queue når vi er færdige med den nuværende operation (??)
- Hvis operationen er ADDV skal vi lægge en ny destination i address queue hver gang vi skifter subvector
- Hvis operationen er DOT, MVP eller SUM skal vi kun lægge en ny destination ind når vi starter en helt ny DOT, MVP, SUM

-
 */
/**
 * A wrapper around the matrix processing unit, holding the MPU as well the hardcoded memory block(s) holding the
 * KE-matrix used for a large number of operations.
 * This is also where multiplexers are used to select between sending the same element from the B vector to all PE's
 * in the MPU, or sending `nelem` separate values at once.
 *
 * The MPU may only perform one kind of operation at once. The current operation type is output on
 * [[MPUWrapIO.MPUWrapOutput.opType]]. A new kind of operation may only be put into the MPU when [[MPUWrapIO.MPUWrapOutput.queueEmpty]] is true.
 * If two different kinds of operations are attempted at once, the output values are undefined.
 * Ensuring that this constraint is satisfied is done by the control module (not seen anywhere yet)
 * @param nelem The number of processing elements to instantiate
 */
class MPUWrapper(val nelem: Int) extends Module {
  val io = IO(new MPUWrapIO(nelem))


  val ready = Wire(Bool())
  val valid = Wire(Bool())
  val MPU = Module(new MatrixProcessingUnit(nelem))
  val KE = Module(new KEWrapper(nelem))
  val destinationQueue = Module(new util.Queue(UInt(), 16))
  val currentOpType = RegInit(NOP)
  val X = RegInit(0.U(log2Ceil(24/NUM_PROCELEM+1).W))  //X-coordinate into KE-wrapper
  val Y = RegInit(0.U(log2Ceil(24/NUM_PROCELEM+1).W)) //Y-coordinate into KE-wrapper
  val col = RegInit(0.U(log2Ceil(NUM_PROCELEM+1).W)) //Column selection from KE submatrix, OR subvector selection

  val in = RegEnable(io.in, !io.in.stall && ready) //We only load in values when we're ready and not being stalled by the control unit

  //Shorthands and multiplexer selects. Most of these should be placed in front of registers.
  val opReg = RegInit(MPUopcode.NOP)
  opReg := Mux(ready, Mux(io.in.stall, MPUopcode.NOP, io.in.op), opReg)

  val vectorLength = in.vectorLength //Total length of the vector being processed.
  val vectorElementsProcessed = RegInit(0.U(log2Ceil(VREG_DEPTH+1).W))

  ready := !((vectorLength - vectorElementsProcessed) > nelem.U)
  valid := !ready || (ready && col =/= 0.U)

  //Input a,b vectors into MPU
  val mpu_a = Wire(Vec(nelem, SInt(FIXED_WIDTH.W)))
  val mpu_b = Wire(Vec(nelem, SInt(FIXED_WIDTH.W)))

  //MPU Selection bits
  val sbg = Module(new control.SelectionBitsGenerator)
  sbg.io.diff := vectorLength - vectorElementsProcessed
  sbg.io.length := vectorLength
  sbg.io.processed := vectorElementsProcessed


  //This makes it easier to address subvectors of the input vectors a, b
  val a_subvec = Wire(Vec(VREG_DEPTH/nelem, Vec(nelem, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(VREG_DEPTH/nelem, Vec(nelem, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until VREG_DEPTH/nelem) {
    a_subvec(i) := io.in.a.slice(i*nelem, (i+1)*nelem)
    b_subvec(i) := io.in.b.slice(i*nelem, (i+1)*nelem)
  }

  //Subvector selection into MPU
  when(opReg === MPUopcode.ADDV) {
    mpu_a := a_subvec(col)
    mpu_b := b_subvec(col)
    MPU.io.in.op := Opcode.ADD
  } .otherwise {
    mpu_a := VecInit(Seq.fill(nelem)(0.S(FIXED_WIDTH.W)))
    mpu_b := VecInit(Seq.fill(nelem)(0.S(FIXED_WIDTH.W)))
    MPU.io.in.op := Opcode.NOP
  }

  //Update col and vectorElementsProcessed
  when(opReg === MPUopcode.ADDV) {
    when(!ready) { //Still processing full slices
      vectorElementsProcessed := vectorElementsProcessed + nelem.U
      col := col + 1.U
    } .elsewhen(io.in.stall) {
      vectorElementsProcessed := vectorLength
    } .otherwise { //Reset registers for next time around if data is incoming
      vectorElementsProcessed := 0.U
      col := 0.U
    }
  }

  for(i <- 0 until NUM_PROCELEM) {
    MPU.io.in.a(i) := Mux(sbg.io.selectBits(i), mpu_a(i), 0.S)
    MPU.io.in.b(i) := Mux(sbg.io.selectBits(i), mpu_b(i), 0.S)
  }
  MPU.io.in.valid := valid
  MPU.io.in.macLimit := in.macLimit



  io.out.opType := in.op
  io.out.queueEmpty := destinationQueue.io.count === 0.U
  io.out.ready := ready

  //Enqueue destination elements whenever an operation occurs
  destinationQueue.io.enq.bits := io.in.rd
  destinationQueue.io.enq.valid := valid//TODO this is probably the wrong statement

  //Dequeue destination elements whenever a result is produced
  destinationQueue.io.deq.ready := MPU.io.out.valid
  io.out.valid := MPU.io.out.valid
  io.out.res := MPU.io.out.res
  io.out.rd := destinationQueue.io.deq.bits

  //KE matrix
  KE.io.keY := Y
  KE.io.keX := X
  KE.io.keCol := col

}

/**
 * I/O ports for the [[MPUWrapper]]. Generally implements the same ports as [[MatrixProcessingUnit]],
 * but there are some slight differences as of right now (mostly with how operations are encoded)
 * @param nelem
 */
class MPUWrapIO(val nelem: Int) extends Bundle {
  val in = Input(new MPUWrapInput(nelem))
  val out = Output(new MPUWrapOutput(nelem))

  /**
   * Input ports for the MPU wrapper
   * @param nelem Number of processing elements to be instantiated / number of operands per operation
   */
  class MPUWrapInput(val nelem: Int) extends Bundle {
    /** Vector of first operands (if KE-matrix isn't used) */
    val a = Vec(VREG_DEPTH, SInt(FIXED_WIDTH.W))
    /** Vector of second operands */
    val b = Vec(VREG_DEPTH, SInt(FIXED_WIDTH.W))
    /** Opcode. See [[MPUopcode]] */
    val op = MPUopcode()
    /** Destination register for result when produced */
    val rd = UInt(log2Ceil(NUM_VREG+1).W)
    /** Number of elements in the two vectors being operated on */
    val vectorLength = UInt(log2Ceil(VREG_DEPTH+1).W)
    /** Number of multiply-accumulates to perform if the operation specified is a MAC */
    val macLimit = UInt(32.W)
    /** Indicates that the pipeline should be stalled, converting all incoming operations to NOPs */
    val stall = Bool()
  }

  /**
   * Output ports for the MPU wrapper
   * @param nelem Number of processing elements to be instantiated / number of operands per operation
   */
  class MPUWrapOutput(val nelem: Int) extends Bundle {
    /** Vector of results produced by the processing elements */
    val res = Vec(nelem, SInt(FIXED_WIDTH.W))
    /** Destination register for the result produced */
    val rd = UInt(log2Ceil(NUM_VREG+1).W)
    /** Asserted when the outputs are valid */
    val valid = Bool()
    /** Type of operation currently being processed in the MPU. Only operations of the same type may be processed at once*/
    val opType = MPUopcode()
    /** True if the destinationQueue is empty, false if not */
    val queueEmpty = UInt()
    /** True if the execution pipeline is ready to receive new data */
    val ready = Bool()
  }
}
/*
TODO: We need to decide on the way that the MPU should be structued. Do we want a grid, or a line?
  It looks as if grid/line will work the same when doing matrix operations, since the same number of PE's allows
  for the same number of calculations pr. cycle.
  Opting for a line will make vector-vector operations easier, however

TODO: Rework the operations for processing elements to supporting MAC as well? Decide on a good schema
  Alternatively, this can be saved for later, when we know how the cache and outside logic works?
 */

object MPUopcode extends ChiselEnum {
  val NOP, ADDV, SUBV, MULV, MULSV, DIVV, DOT, MVP, SUM = Value
  /*
  ADDV: Elementwise addition
  SUBV: Elementwise subtraction
  MULV: Elementwise multiplication
  DIVV: Elementwise division
  DOT: Vector dot product
  MVP: Matrix-vector product (uses the built-in KE matrix)
  MAC: Multiply-accumulate (probably not necessary)
  SUM: Sum all elements in one vector
   */
  //See https://www.chisel-lang.org/chisel3/docs/explanations/chisel-enum.html for documentation on using ChiselEnum
}
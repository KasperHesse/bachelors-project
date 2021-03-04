package vector

import arithmetic._
import chisel3._
import utils.Fixed._
import ProcessingElement._

/**
 * I/O Ports for a processing elements.
 */
class ProcElemIO extends Bundle {
  val in = Input(new ProcElemInput)
  val out = Output(new ProcElemOutput)

  /**
   * Input ports for a processing element.
   */
  class ProcElemInput extends Bundle {
    /** First operand. Is the numerator when performing division operations */
    val a = SInt(FIXED_WIDTH.W)
    /** Second operand. Is the denominator when performing division operations */
    val b = SInt(FIXED_WIDTH.W)
    /** Enable signal. Should be asserted for one clock cycle when the operands are valid */
    val en = Bool()
    /** Operation to perform. See [[ProcessingElement]] for the operations available */
    val op = UInt(5.W)
  }

  /**
   * Output ports for a processing element
   */
  class ProcElemOutput extends Bundle {
    /** Result of the operation */
    val res = SInt(FIXED_WIDTH.W)
    /** Signals that the operations is finished and a valid signal can be sampled. High for one clock cycle */
    val done = Bool()
  }

}


// Takes up 36/342 DSP blocks and 1387/113560 ALM's when used with 54-bit values
// Takes up 82/342 DSP blocks and 2469/113560 ALM's when used with 64-bit values
/*
MAC Operations: Once mac is asserted, keep the value 0 on the output until internal 'done' signal appears.
When this happens, start multiplying and accumulating the values found
As long as MAC is kept high, keep accumulating whenever 'done' is asserted, otherwise simply add the current accumuland and 0
When MAC is deasserted, wait until  'done' output goes low, and then output the final result?
 */
/**
 * A processing element, used in the matrix/vector unit [[MatrixProcessingUnit]] to perform vector operations. Implements [[ProcElemIO]].
 */
class ProcessingElement extends Module {
  val io = IO(new ProcElemIO)

  //Latch inputs
  val in = RegNext(io.in)

  //Modules in use
  val mul = Module(FixedPointMul(utils.MulTypes.SINGLECYCLE))
  val div = Module(FixedPointDiv(utils.DivTypes.NEWTONRAPHSON))
  val asu = Module(new FixedPointAddSub)

  //Multiplier and divider. Inputs and outputs up to middle register
  mul.io.a := in.a
  mul.io.b := in.b
  mul.io.en := in.en

  div.io.in.numer := in.a
  div.io.in.denom := in.b
  div.io.in.en := in.en

  //Middle register stage
  val mulDivRes = Mux(in.op(DIV_P), div.io.out.res, mul.io.res)
  val mulDivDone = Mux(in.op(DIV_P), div.io.out.done, mul.io.done)
  val mulDivResultReg = RegNext(mulDivRes)
  val mulDivDoneReg = RegNext(mulDivDone)
  val aReg = RegNext(in.a)
  val bReg = RegNext(in.b)
  val opReg = RegNext(in.op)
  val enReg = RegNext(in.en)

  //Add/sub and result register.
  val asu_a = Wire(SInt(FIXED_WIDTH.W))
  val asu_b = Wire(SInt(FIXED_WIDTH.W))
  asu.io.in.a := asu_a
  asu.io.in.b := asu_b
  asu.io.in.op := opReg(SUB_P) //All other operations should addt the operands
  asu.io.in.en := DontCare
  val resReg = RegNext(asu.io.out.res)

  //Signal multiplexers for asu and result reg
  when(in.op(ADD_P) || in.op(SUB_P)) {
    asu_a := aReg
    asu_b := bReg
  } .elsewhen(in.op(MAC_P)) {
    asu_a := resReg
    asu_b := mulDivResultReg
  } .otherwise { //mul, div, other operations
    asu_a := 0.S(FIXED_WIDTH.W)
    asu_b := mulDivResultReg
  }

  //Outputs
  io.out.res := asu.io.out.res
  io.out.done := Mux(opReg(MUL_P) || opReg(DIV_P), mulDivDoneReg, enReg)

}

//TODO: Rework the opcodes to make use of the ChiselEnum class (chisel3.experimental)
object ProcessingElement {
  val ADD = 1.U(5.W)
  val SUB = 2.U(5.W)
  val MUL = 4.U(5.W)
  val DIV = 8.U(5.W)
  val MAC = 16.U(5.W)

  val ADD_P = 0
  val SUB_P = 1
  val MUL_P = 2
  val DIV_P = 3
  val MAC_P = 4
}

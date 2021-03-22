package vector

import arithmetic._
import chisel3._
import utils.Fixed._
//import ProcElemOpcode._
import chisel3.util.RegEnable
import Opcode._

//Takes up 28/342 DSP blocks when used with 2 stage3-reps
//Allows us to work with 12 of these bad-boys, aww yeah
// Takes up 36/342 DSP blocks and 1387/113560 ALM's when used with 54-bit values
/*
MAC Operations: Once mac is asserted, keep the value 0 on the output until internal 'done' signal appears.
When this happens, start multiplying and accumulating the values found
As long as MAC is kept high, keep accumulating whenever 'done' is asserted, otherwise simply add the current accumuland and 0
When MAC is deasserted, wait until  'done' output goes low, and then output the final result?
 */
/**
 * A processing element, used in the matrix/vector unit [[MatrixProcessingUnit]] to perform vector operations. Implements [[ProcElemIO]].
 * Presents the output of an add/sub/mul/div operation as soon as the result is available. If a MAC operation is performed, the input [[ProcElemIO.ProcElemInput.macLimit]] decides
 * how many additions should be performed before the output is presented.
 * @note once MAC is asserted, the system will keep accumulating until macLimit additions have been performed. At this stage, it will move to another state if told so.
 * If another operation is asserted while a MAC is processing, the output of that operation will be lost
 */
class ProcessingElement extends Module {
  val io = IO(new ProcElemIO)

  //Latch inputs
//  val in = RegNext(io.in)

  val in = io.in
  //Modules in use
  val mul = Module(FixedPointMul(utils.MulTypes.SINGLECYCLE))
  val div = Module(FixedPointDiv(utils.DivTypes.NEWTONRAPHSON))
  val asu = Module(new FixedPointAddSub)

  //Multiplier and divider. Inputs and outputs up to middle register
  mul.io.in.a := in.a
  mul.io.in.b := in.b
  mul.io.in.valid := in.valid && in.op === MUL

  div.io.in.numer := in.a
  div.io.in.denom := in.b
  div.io.in.valid := in.valid && in.op === DIV

  //Middle register stage. Default to taking multiplier output (useful for MAC operations)
  val mulDivRes = Mux(in.op === DIV, div.io.out.res, mul.io.out.res)
  val mulDivValid = Mux(in.op === DIV, div.io.out.valid, mul.io.out.valid)
  val mulDivResultReg = RegNext(mulDivRes)
  val mulDivValidReg = RegNext(mulDivValid)
  val tmp = RegNext(in)


  //Add/sub and result register.
  val asu_a = Wire(SInt(FIXED_WIDTH.W))
  val asu_b = Wire(SInt(FIXED_WIDTH.W))
  asu.io.in.a := asu_a
  asu.io.in.b := asu_b
  asu.io.in.op := tmp.op === SUB //All other operations should add the operands
  asu.io.in.valid := Mux(tmp.op === ADD || tmp.op === SUB, tmp.valid, mulDivValidReg)
  val resReg = RegEnable(asu.io.out.res, asu.io.out.valid)
  val macLimit = RegInit(0.U(32.W))
  val macCnt = RegInit(0.U(32.W))


  //Signal multiplexers for asu and result reg
  when(tmp.op === ADD || tmp.op === SUB) {
    asu_a := tmp.a
    asu_b := tmp.b
  } .elsewhen(tmp.op === MAC) {
    asu_a := Mux(macCnt === 0.U, 0.S(FIXED_WIDTH.W), resReg) //Resets resReg when restarting a MAC operation
    asu_b := mulDivResultReg
  } .otherwise { //mul, div, other operations
    asu_a := 0.S(FIXED_WIDTH.W)
    asu_b := mulDivResultReg
  }

  //Update macCnt and macLimit when necessary
  //macLimit is set when macCnt=0 and a MAC operation is started
  //macCnt is updated on each operation where op=MAC and en=1
  macLimit := Mux(macCnt === 0.U && tmp.valid, tmp.macLimit, macLimit)
  macCnt := Mux(tmp.op === MAC && tmp.valid, Mux(macCnt === macLimit-1.U, 0.U, macCnt + 1.U), macCnt)
  //Outputs
  io.out.res := asu.io.out.res
  //When processing MAC, MAC_P will always be high
  io.out.valid := Mux(tmp.op === MAC, macCnt === macLimit-1.U && asu.io.out.valid, asu.io.out.valid)
}

/**
 * I/O Ports for a [[ProcessingElement]].
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
    /** Data valid signal. Should be asserted for one clock cycle when the operands are valid */
    val valid = Bool()
    /** Operation to perform. See [[ProcessingElement]] for the operations available */
//    val op = UInt(ProcElemOpcode.OP_WIDTH.W)
    val op = Opcode()
    /** The number of multiply/accumulates to perform before the summation is finished and the output should be presented.
     * This value should be set in the same clock cycle as op=MAC and valid=true to set the internal register. */
    val macLimit = UInt(32.W) //Magic number for right now
  }

  /**
   * Output ports for a processing element
   */
  class ProcElemOutput extends Bundle {
    /** Result of the operation */
    val res = SInt(FIXED_WIDTH.W)
    /** Data valid signal. Signals that the operations is finished and a valid signal can be sampled. High for one clock cycle */
    val valid = Bool()
  }
}

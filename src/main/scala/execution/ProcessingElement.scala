package execution

import arithmetic._
import chisel3._
import chisel3.util.log2Ceil
import utils.Config.{NDOFLENGTH, NUM_PROCELEM}
import utils.Fixed._
//import ProcElemOpcode._
import chisel3.util.RegEnable
import execution.Opcode._

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

  val in = RegNext(io.in)
  //Modules in use
  val mul = Module(FixedPointMul(utils.MulTypes.MULTICYCLE))
  val div = Module(FixedPointDiv(utils.DivTypes.NEWTONRAPHSON))
  val alu = Module(new FixedPointALU)

  // --- STAGE 1, MULTIPLIER AND DIVISION ---
  mul.io.in.a := in.a
  mul.io.in.b := in.b
  mul.io.in.valid := in.valid && (in.op === MUL || in.op === MAC || in.op === RED)

  div.io.in.numer := in.a
  div.io.in.denom := in.b
  div.io.in.valid := in.valid && in.op === DIV

  // --- STAGE 2, ADD/SUB ---
  //Middle register stage. Default to taking multiplier output (useful for MAC operations)
  val mulDivRes = Mux(in.op === DIV, div.io.out.res, mul.io.out.res)
  val mulDivValid = Mux(in.op === DIV, div.io.out.valid, mul.io.out.valid)
  val mulDivResultReg = RegNext(mulDivRes)
  val mulDivValidReg = RegNext(mulDivValid)
  val tmp = RegNext(RegNext(in)) //Double regnext since multi-cycle multipliers have 1 register stage inside


  // --- ARITHMETIC AND MAC RESULT REGISTER ---
  val alu_a = Wire(SInt(FIXED_WIDTH.W))
  val alu_b = Wire(SInt(FIXED_WIDTH.W))
  alu.io.in.a := alu_a
  alu.io.in.b := alu_b
  alu.io.in.op := tmp.op //All operations not SUB, MAX, MIN, ABS will add the operands
  alu.io.in.valid := Mux(tmp.op === ADD || tmp.op === SUB || tmp.op === MAX || tmp.op === MIN || tmp.op === ABS, tmp.valid, mulDivValidReg)
  val resReg = RegEnable(alu.io.out.res, alu.io.out.valid && (tmp.op === MAC || tmp.op === RED))
  val macLimit = RegInit(0.U(log2Ceil(NDOFLENGTH/NUM_PROCELEM+1).W))
  val macCnt = RegInit(0.U(log2Ceil(NDOFLENGTH/NUM_PROCELEM+1).W))


  //Signal multiplexers for alu and result reg
  when(tmp.op === ADD || tmp.op === SUB || tmp.op === MAX || tmp.op === MIN || tmp.op === ABS) {
    alu_a := tmp.a
    alu_b := tmp.b
  } .elsewhen(tmp.op === MAC || tmp.op === RED) {
    alu_a := Mux(macCnt === 0.U, 0.S(FIXED_WIDTH.W), resReg) //Resets resReg when restarting a MAC operation
    alu_b := mulDivResultReg
  } .otherwise { //mul, div, other operations
    alu_a := 0.S(FIXED_WIDTH.W)
    alu_b := mulDivResultReg
  }

  //macLimit is set when macCnt=0 and a MAC or RED operation is started
  //macCnt is updated on each operation where op=MAC or RED and valid
  macLimit := Mux(macCnt === 0.U, tmp.macLimit, macLimit)
  macCnt := Mux((tmp.op === MAC || tmp.op === RED) && tmp.valid, Mux(macCnt === macLimit-1.U, 0.U, macCnt + 1.U), macCnt)
  when(macCnt === macLimit-1.U &&  tmp.valid && (tmp.op === MAC || tmp.op === RED)) {
    //Reset result register after each MAC operation
    resReg := 0.S(FIXED_WIDTH.W)
  }
  //Outputs
  io.out.res := alu.io.out.res
  io.out.valid := Mux(tmp.op === MAC || tmp.op === RED, macCnt === macLimit-1.U && alu.io.out.valid, alu.io.out.valid)
  io.out.op := tmp.op
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
    val op = Opcode()
    /** The number of multiply/accumulates to perform before the summation is finished and the output should be presented.
     * This value should be set in the same clock cycle as op=MAC and valid=true to set the internal register. */
    val macLimit = UInt(log2Ceil(NDOFLENGTH/NUM_PROCELEM+1).W)
  }

  /**
   * Output ports for a processing element
   */
  class ProcElemOutput extends Bundle {
    /** Result of the operation */
    val res = SInt(FIXED_WIDTH.W)
    /** Data valid signal. Signals that the operations is finished and a valid signal can be sampled. High for one clock cycle */
    val valid = Bool()
    /** The opcode corresponding to the currently generated result */
    val op = Opcode()
  }
}

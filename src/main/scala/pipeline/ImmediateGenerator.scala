package pipeline

import chisel3._
import utils.Fixed._
import chisel3.util.Cat

/**
 * I/O ports for immediate generation module. Inputs are only unsigned to as to correlate easily with the field
 * definitions in R-type instruction fields. They will be parsed as signed.
 */
class ImmGenIO extends Bundle {
  val int = Input(UInt(4.W))
  val frac = Input(UInt(7.W))
  val imm = Output(SInt(FIXED_WIDTH.W))
}

/**
 * Immediate generator. Takes the imm1 field as well as rs2/imm2 field and constructs an immediate value
 * The rs2 field is used for integer part. The imm1 field is used for the fractional part. Implements [[ImmGenIO]]
 */
class ImmediateGenerator extends Module {
  val io = IO(new ImmGenIO)

  //Number of bits to add at the top and bottom to pad to correct length
  val topPadding = (INT_WIDTH + 1) - 3 //-3 because we have three bits of integer value when using 4 bits of imm1
  val bottomPadding = FRAC_WIDTH - 7 //-7 because we have three bits remaining from imm1 + 4 bits from rs2

  val sign = io.int(3)
  val top = Mux(sign, ((1 << topPadding) - 1).U(topPadding.W), 0.U(topPadding.W))
  val bot = 0.U(bottomPadding.W)
  val imm = Cat(Seq(top, io.int, io.frac, bot))

  io.imm := imm.asSInt()
}

package utils

import chisel3._
import chisel3.util._
import Fixed.FIXED_WIDTH
import utils.Config.leastMultiple

/**
 * A module for testing how to use the UART module
 * 8 SINT(FIXED_WIDTH) are stored in a buffer and gradually transmitted on the UART as possible
 */
class UseUart extends Module {
  val io = IO(new Bundle {
    val txd = Output(Bits(1.W))
  })

  val uart = Module(new BufferedTx(20, 5))
  val data = RegInit(VecInit(Seq("hf1234".U(20.W).asSInt, "h15678".U(20.W).asSInt, "h29abc".U(20.W).asSInt, "h3def0".U(20.W).asSInt)))

  val bytesPerWord: Int = (20.toDouble / 8).ceil.toInt
  val byteCnt = RegInit((bytesPerWord-1).U)

  val dataCnt = RegInit(0.U(2.W))

  //Always output 8 LSB of current data word
  uart.io.channel.data := data(dataCnt)(7,0)
  //Output is always valid
  uart.io.channel.valid := true.B

  when(uart.io.channel.ready) {
    data(dataCnt) := data(dataCnt) >> 8
    //Reduce number of bytes that must be transmitted in this data word
    byteCnt := Mux(byteCnt === 0.U, (bytesPerWord-1).U, byteCnt - 1.U)

    //Word to transmit only changes when bytecnt is 0 and uart is ready for next byte
    dataCnt := Mux(byteCnt === 0.U, dataCnt + 1.U, dataCnt)
  }

  io.txd := uart.io.txd
}

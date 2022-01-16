package utils

import chisel3._
import chisel3.util._
import Fixed.{FIXED_WIDTH, double2fixed}
import execution.{StypeBaseAddress, StypeMod}
import memory.UartTransmitter
import utils.Config.{NUM_MEMORY_BANKS, leastMultiple}

/**
 * A module for testing how to use the UART module
 * 8 SINT(FIXED_WIDTH) are stored in a buffer and gradually transmitted on the UART as possible
 */
class UseUart extends Module {
  val io = IO(new Bundle {
    val txd = Output(Bits(1.W))
  })

  val uart = Module(new UartTransmitter())

  val cnt = RegInit(0.U(4.W))

  uart.io.id.valid := cnt === 2.U
  uart.io.wrData.valid := cnt === 3.U

  cnt := Mux(cnt =/= 15.U, cnt + 1.U, cnt)

  uart.io.id.bits.baseAddr := StypeBaseAddress.UART
  for(i <- 0 until NUM_MEMORY_BANKS) {
    uart.io.id.bits.indices(i) := (i).U
    uart.io.id.bits.validIndices(i) := true.B
  }
  val wd = Seq[Double](1, 3.14, -1, 13.37, -13.37, 0.2, -2048, 28)
  for(i <- 0 until 8) {
    uart.io.wrData.bits.wrData(i) := double2fixed(wd(i)).S(FIXED_WIDTH.W)
  }

//  for(i <- 0 to 3) {
//    uart.io.wrData.bits.wrData(2*i) := (i+1).S(FIXED_WIDTH.W)
//    uart.io.wrData.bits.wrData(2*i+1) := 0x123456789abcdL.S(FIXED_WIDTH.W)
//  }
  uart.io.wrData.bits.mod := StypeMod.VEC
  uart.io.wrData.bits.iter := 0.U
  io.txd := uart.io.txd
}

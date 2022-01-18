package memory

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util._
import utils.BufferedTx
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import execution.{IdMemIO, RegisterFileType, StypeBaseAddress, StypeLoadStore, StypeMod, WbIdIO}

class UartTransmitterIO extends Bundle {
  /** UART output data */
  val txd = Output(Bits(1.W))
  /** Input instruction from id stage for VEC operations */
  val vec = Flipped(Decoupled(new AddressGenProducerIO))
  /** Input data from ID stage for SEL operations */
  val sel = Flipped(Decoupled(new IJKgeneratorConsumerIO))
  /** Input write data from id stage*/
  val wrData = Flipped(Decoupled(new WriteQueueBundle))
}
/**
 * UART Transmitter used to transmit data off the FPGA
 * Uses the [[utils.BufferedTx]] implementation of a UART transmitter to transmit data.
 * When a st.vec instruction to memory address UART is performed, this module intercepts the instruction,
 * writing the data being stored onto the UART port
 */
class UartTransmitter(val clkFreq: Int = 50e6.toInt, val baudRate: Int = 115200) extends Module {
  val io = IO(new UartTransmitterIO)
  val uart = Module(new BufferedTx(clkFreq, baudRate))

  /** Buffer for storing X-values before transmitting over UART */
  val buffer = Reg(Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W)))

  /** Number of bytes to transmit for each fixed-point value */
  val bytesPerValue: Int = (FIXED_WIDTH.toDouble / 8).ceil.toInt

  /** Number of bytes transmitted of the current value */
  val byteCnt = RegInit(0.U(log2Ceil(bytesPerValue+1).W))

  /** How many values in the buffer that have been transmitted */
  val dataCnt = RegInit(0.U(log2Ceil(NUM_MEMORY_BANKS).W))

  val idle :: loadVec :: loadSel :: transmit :: Nil = Enum(4)
  /** State of transmitter */
  val state = RegInit(idle)

  // Next state logic
  switch(state) {
    is(idle) {
      when(io.vec.valid && io.vec.bits.baseAddr === StypeBaseAddress.UART) {
        state := loadVec
      } .elsewhen(io.sel.valid && io.sel.bits.baseAddr === StypeBaseAddress.UART) {
        state := loadSel
      }
    }
    is(loadVec) {
      buffer := io.wrData.bits.wrData
      state := transmit
    }
    is(loadSel) {
      buffer := io.wrData.bits.wrData
      state := transmit
      dataCnt := (NUM_MEMORY_BANKS-1).U //We only wish to transmit one value
    }
    is(transmit) {
      when(dataCnt === (NUM_MEMORY_BANKS-1).U && byteCnt === (bytesPerValue-1).U && uart.io.channel.ready) {
        dataCnt := 0.U
        byteCnt := 0.U
        state := idle
      }
    }
  }

  //UART output data logic
  //We transfer the 8 LSB of current value to tx buffer
  val shift = buffer(dataCnt) >> 8
  when(uart.io.channel.ready && state === transmit) {
    buffer(dataCnt) := shift

    //Reduce number of bytes to transmit in this data word
    byteCnt := Mux(byteCnt === (bytesPerValue-1).U, 0.U, byteCnt + 1.U)

    //Increment which element in buffer to transmit
    dataCnt := Mux(byteCnt === (bytesPerValue-1).U, dataCnt + 1.U, dataCnt)
    //Once we reach final element, next-state logic will take us to idle state where all registers will be correctly reset
  }

  // -- CONNECTIONS --
  //Data going into uart
  uart.io.channel.data := buffer(dataCnt)(7,0)
  uart.io.channel.valid := state === transmit

  //Signals back to id stage
  io.vec.ready := state === idle
  io.sel.ready := state === idle
  io.wrData.ready := (state === idle) || (state === loadSel) || (state === loadVec)

  //Output to world
  io.txd := uart.io.txd


}

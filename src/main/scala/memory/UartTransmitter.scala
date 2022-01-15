package memory

import chisel3._
import chisel3.experimental.BundleLiterals._
import chisel3.util.{Cat, Enum, log2Ceil}
import utils.BufferedTx
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import execution.{IdMemIO, RegisterFileType, StypeBaseAddress, StypeLoadStore, StypeMod, WbIdIO}

/**
 * UART Transmitter used to transmit data off the FPGA
 * Uses the [[utils.BufferedTx]] implementation of a UART transmitter to transmit data.
 * Sits outside the memory stage, snooping on the output of the memory writeback stage.
 * When enabled with the *ustart* instruction, will connect to the input of the memory stage, reading data from memory
 */
class UartTransmitter(val clkFreq: Int = 50e6.toInt, val baudRate: Int = 115200) extends Module {
  val io = IO(new Bundle {
    /** UART output data */
    val txd = Output(Bits(1.W))
    /** Enable signal, signalling that values should be sampled */
    val en = Input(Bool())
    /** Data from writeback stage */
    val wbid = Flipped(new WbIdIO)

    val idmem = new IdMemIO
  })
  val uart = Module(new BufferedTx(clkFreq, baudRate))

  /** Buffer for storing X-values before transmitting over UART */
  //Has VREG_DEPTH elements, the amount of elements transmitted over the writeback bus
  val buffer = Reg(Vec(VREG_DEPTH, SInt(FIXED_WIDTH.W)))

  /** Number of bytes to transmit for each fixed-point value */
  val bytesPerValue: Int = (FIXED_WIDTH.toDouble / 8).ceil.toInt

  /** Number of bytes remaining in the current value to transmit */
  val byteCnt = RegInit((bytesPerValue-1).U)

  /** How many values in the buffer that have been transmitted */
  val dataCnt = RegInit(0.U(log2Ceil(VREG_DEPTH).W))

  val idle :: load :: transmit :: Nil = Enum(3)

  /** State of transmitter */
  val state = RegInit(idle)

  //Address generation logic
  /** Base address to be accessed */
  val baseAddr = StypeBaseAddress.XPHYS
  /** Which base index in that base address to access. Used to generate indices below */
  val baseIndex = RegInit(0.U(log2Ceil(NDOF+1).W))
  /** The [[NUM_MEMORY_BANKS]] indices to access, based on the baseIndex */
  val indices = Wire(Vec(NUM_MEMORY_BANKS, UInt(log2Ceil(NDOF+1).W)))
  /** Valid signal for vec-interface handshake */
  val validFlag = WireDefault(false.B)
  /** Read queue bundle used to access data */
  val rq = Wire(new ReadQueueBundle)
  rq.mod := StypeMod.VEC
  rq.iter := 0.U
  rq.rd.reg := 0.U
  rq.rd.subvec := 0.U
  rq.rd.rf := RegisterFileType.VREG
  rq.rd.rfUint := RegisterFileType.VREG.litValue.U

  /** How many reads have been issued for this instruction */
  val cnt = RegInit(0.U(log2Ceil(VREG_DEPTH/NUM_MEMORY_BANKS).W))


  //Output generation logic in load state
  //When in the load state, send out three data read packets
  when(state === load) {
    //Using < to generate exactly (VREG_DEPTH/NUM_MEM_BANKS)-1 valid instructions
    validFlag := cnt < (VREG_DEPTH/NUM_MEMORY_BANKS).U
    when(io.idmem.vec.ready) {
      cnt := cnt + 1.U
      baseIndex := baseIndex + 8.U

      //Once we is asserted, the instructions have been processed, and we're about
      //to move to the transmit state
      when(io.wbid.we) {
        cnt := 0.U
      }
    }
  }

  //Next state logic
  when(state === idle) {
    dataCnt := 0.U
    byteCnt := (bytesPerValue-1).U
    when(io.en && !RegNext(io.en)) { //On rising edge
      state := load
    }
  } .elsewhen(state === load) {
    when(io.wbid.we) {
      buffer := io.wbid.wrData
      state := transmit
    }
  } .elsewhen(state === transmit) {
    //End of transmitting packet
    when(byteCnt === 0.U && dataCnt === (VREG_DEPTH-1).U) {
      //If path taken, we're not yet finished processing all elements in the vector
      when(baseIndex =/= NELEMLENGTH.U) {
        state := load
      } .otherwise { //If taken, we're completely finished
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
    byteCnt := Mux(byteCnt === 0.U, (bytesPerValue-1).U, byteCnt - 1.U)

    //Increment which element in buffer to transmit
    dataCnt := Mux(byteCnt === 0.U, Mux(dataCnt === (VREG_DEPTH-1).U, 0.U, dataCnt + 1.U), dataCnt)
    //Once we reach final element, next-state logic will take us to idle state where all registers will be correctly reset
  }

  // -- CONNECTIONS --
  //Data going into uart
  uart.io.channel.data := buffer(dataCnt)(7,0)
  uart.io.channel.valid := state === transmit

  //Output to memory stage
  io.idmem.edof := DontCare
  io.idmem.edof.valid := false.B
  io.idmem.neighbour := DontCare
  io.idmem.neighbour.valid := false.B
  io.idmem.writeQueue := DontCare
  io.idmem.writeQueue.valid := false.B

  io.idmem.ls := StypeLoadStore.LOAD

  io.idmem.readQueue.bits := rq
  io.idmem.readQueue.valid := validFlag & io.idmem.vec.ready


  for(i <- 0 until NUM_MEMORY_BANKS) {
    indices(i) := baseIndex | i.U //Index is always a multiple of 8, so we can simply concatenate in the i-value
  }
  io.idmem.vec.bits.indices := indices
  io.idmem.vec.bits.baseAddr := baseAddr
  io.idmem.vec.bits.validIndices := VecInit(Seq.fill(8)(true.B)) //always valid indices
  io.idmem.vec.valid := validFlag


  //Output to world
  io.txd := uart.io.txd
}

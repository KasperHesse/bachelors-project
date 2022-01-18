
import chisel3._
import memory.{MemWbIO, MemoryStage, UartTransmitter}
import execution._
import utils.TimingWrapper

/**
 * The top level module for the topology optimizer
 * @param IMsize The number of instructions in instruction memory
 * @param IMinitFileLocation Location of the instruction memory initialization file
 * @param wordsPerBank Number of elements in each memory bank
 * @param memInitFileLocation Location of memory initialization files. This is the relative path. Each memory bank initialization file must be named
 *                            'membank_x.txt', where x is in the range [0;NUM_MEMORY_BANKS[. If eg 'resources/meminit' is given as parameter, the first file should be named 'resources/meminit/membank_0.txt'
 */
class TopLevel(IMsize: Int, IMinitFileLocation: String, wordsPerBank: Int, memInitFileLocation: String, clkFreq: Int = 50e6.toInt) extends Module {
  val io = IO(new Bundle {
    val timing = new TimingOutput(clkFreq)
    val txd = Output(Bool())
  })

  val fetch = Module(new Fetch(IMsize, IMinitFileLocation))
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val writeback = Module(new Writeback)
  val forward = Module(new Forwarding)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)
  val timing = Module(new TimingWrapper(clkFreq))
  val uart = Module(new UartTransmitter(clkFreq, baudRate = 115200))


  fetch.io.id <> decode.io.fe
  decode.io.ex <> execute.io.id
  execute.io.wb <> writeback.io.ex
  writeback.io.id <> decode.io.wb
  writeback.io.fwd <> forward.io.wb
  forward.io.ex <> execute.io.fwd
  decode.io.mem <> mem.io.id
  decode.io.memWb <> mem.io.wb
  timing.io.id <> decode.io.time

  control.io.fe <> fetch.io.ctrl
  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.mem <> mem.io.ctrl

  //UART/Memory stage arbitration
  //Valid signals and data can go into uart without arbitration, as the uart also checks for correct base address
  uart.io.wrData.bits := decode.io.mem.writeQueue.bits
  uart.io.wrData.valid := decode.io.mem.writeQueue.valid
  uart.io.vec.bits := decode.io.mem.vec.bits
  uart.io.vec.valid := decode.io.mem.vec.valid
  uart.io.sel.valid := decode.io.mem.neighbour.valid
  uart.io.sel.bits := decode.io.mem.neighbour.bits

  //Ready signal into ID should be taken from uart when baseaddr is UART
  when(decode.io.mem.vec.bits.baseAddr === StypeBaseAddress.UART) {
    decode.io.mem.vec.ready := uart.io.vec.ready
    decode.io.mem.neighbour.ready := uart.io.sel.ready
  } .otherwise {
    decode.io.mem.vec.ready := mem.io.id.vec.ready
    decode.io.mem.neighbour.ready := mem.io.id.neighbour.ready
  }
  //Valid signal into write queue should be disabled when baseAddr is uart
  //Using regnext of baseAddr since wrData arrives one cc after vec base address
  mem.io.id.writeQueue.valid := decode.io.mem.writeQueue.valid && RegNext(decode.io.mem.vec.bits.baseAddr) =/= StypeBaseAddress.UART
  //valid signal into mem.vec and mem.neighbour should be false when addressing uart
  mem.io.id.vec.valid := decode.io.mem.vec.valid && decode.io.mem.vec.bits.baseAddr =/= StypeBaseAddress.UART
  mem.io.id.neighbour.valid := decode.io.mem.neighbour.valid && decode.io.mem.neighbour.bits.baseAddr =/= StypeBaseAddress.UART

  io.timing := timing.io.out
  io.txd := uart.io.txd

}
/**
 * The top level module for the topology optimizer. This module contains additional outputs for simulation purposes
 * @param IMsize The number of instructions in instruction memory
 * @param IMinitFileLocation Location of the instruction memory initialization file
 * @param wordsPerBank Number of elements in each memory bank
 * @param memInitFileLocation Location of memory initialization files. This is the relative path. Each memory bank initialization file must be named
 *                            'membank_x.txt', where x is in the range [0;NUM_MEMORY_BANKS[. If eg 'resources/meminit' is given as parameter, the first file should be named 'resources/meminit/membank_0.txt'
 */
class TopLevelSim(IMsize: Int, IMinitFileLocation: String, wordsPerBank: Int, memInitFileLocation: String, clkFreq: Int = 50e6.toInt) extends Module {
  val io = IO(new Bundle {
    val exout = Output(new ExWbIO)
    val idex = Output(new IdExIO)
    val idctrl = Output(new IdControlIO)
    val wbid = Output(new WbIdIO)
    val idmem = Output(new IdMemIO)
    val memid = Output(new WbIdIO)
    val timing = new TimingOutput(clkFreq)
    val txd = Output(Bool())
  })

  val fetch = Module(new Fetch(IMsize, IMinitFileLocation))
  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val writeback = Module(new Writeback)
  val forward = Module(new Forwarding)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)
  val timing = Module(new TimingWrapper(clkFreq))
  val uart = Module(new UartTransmitter(20, 10))

  fetch.io.id <> decode.io.fe
  decode.io.ex <> execute.io.id
  execute.io.wb <> writeback.io.ex
  writeback.io.id <> decode.io.wb
  writeback.io.fwd <> forward.io.wb
  forward.io.ex <> execute.io.fwd
  decode.io.mem <> mem.io.id
  decode.io.memWb <> mem.io.wb
  timing.io.id <> decode.io.time

  control.io.fe <> fetch.io.ctrl
  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.mem <> mem.io.ctrl

  //UART/Memory stage arbitration
  //Valid signals and data can go into uart without arbitration, as the uart also checks for correct base address
  uart.io.wrData.bits := decode.io.mem.writeQueue.bits
  uart.io.wrData.valid := decode.io.mem.writeQueue.valid
  uart.io.vec.bits := decode.io.mem.vec.bits
  uart.io.vec.valid := decode.io.mem.vec.valid
  uart.io.sel.valid := decode.io.mem.neighbour.valid
  uart.io.sel.bits := decode.io.mem.neighbour.bits

  //Ready signal into ID should be taken from uart when baseaddr is UART
  when(decode.io.mem.vec.bits.baseAddr === StypeBaseAddress.UART) {
    decode.io.mem.vec.ready := uart.io.vec.ready
    decode.io.mem.neighbour.ready := uart.io.sel.ready
  } .otherwise {
    decode.io.mem.vec.ready := mem.io.id.vec.ready
    decode.io.mem.neighbour.ready := mem.io.id.neighbour.ready
  }
  //Valid signal into write queue should be disabled when baseAddr is uart
  //Using regnext of baseAddr since wrData arrives one cc after vec base address
  mem.io.id.writeQueue.valid := decode.io.mem.writeQueue.valid && RegNext(decode.io.mem.vec.bits.baseAddr) =/= StypeBaseAddress.UART
  //valid signal into mem.vec and mem.neighbour should be false when addressing uart
  mem.io.id.vec.valid := decode.io.mem.vec.valid && decode.io.mem.vec.bits.baseAddr =/= StypeBaseAddress.UART
  mem.io.id.neighbour.valid := decode.io.mem.neighbour.valid && decode.io.mem.neighbour.bits.baseAddr =/= StypeBaseAddress.UART


  io.exout <> execute.io.wb

  io.idex := decode.io.ex
  io.idctrl := decode.io.ctrl
  io.idmem := decode.io.mem
  io.wbid := writeback.io.id
  io.memid := mem.io.wb

  io.timing := timing.io.out
  io.txd := uart.io.txd
}

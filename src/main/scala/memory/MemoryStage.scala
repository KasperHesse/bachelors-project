package memory

import chisel3._
import chisel3.util.{Decoupled, Queue, RegEnable, Valid}
import pipeline.StypeMod._
import pipeline.{IdMemIO, MemControlIO, StypeLoadStore, StypeMod, WbIdIO}
import utils.Config._
import utils.Fixed.FIXED_WIDTH

/**
 * I/O ports for the [[MemoryStage]] module.
 */
class MemoryStageIO extends Bundle {
  /** Input connections from decode stage */
  val id = Flipped(new IdMemIO)
  /** Output connections to decode stage */
  val wb = new WbIdIO
  /** Connections to control module */
  val ctrl = new MemControlIO
}

/**
 * A wrapper module around the entirety of the memory stage. Connects to the [[pipeline.Decode]] stage for reads and writes.
 * Implements [[MemoryStageIO]]
 *
 * @param wordsPerBank The number of data words to store in *each* memory bank. The total amount of memory allotted
 *                     is NUM_MEMORY_BANKS*wordsPerBank, each of which is [[utils.Fixed.FIXED_WIDTH]] bits wide.
 * @param memInitFileLocation Location of memory initialization file. Each file will be named 'membank_x.txt', where 'x'
 *                            is a number [0;NUM_MEMORY_BANKS[. The parameter is the relative path to these files, to which the filename is appended.
 *                            If eg memInitFileLocation = "resources/meminit/", one file would be "resources/meminit/membank_0.txt"
 */
class MemoryStage(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new MemoryStageIO)

  // --- MODULES ---
  val addrGen = Module(new AddressGenerator(false))
  val mem = Module(new OnChipMemory(wordsPerBank, memInitFileLocation))
  val wb = Module(new MemoryWriteback)
  val edof = Module(new EdofGenerator)
  val vec = Module(new VecWrapper)
  val indexGen = Module(new IndexGenerator(pipe=true))
  val neighbour = Module(new NeighbourGenerator)
  val readQueue = Module(new Queue(new ReadQueueBundle, 8))
  val writeQueue = Module(new Queue(new WriteQueueBundle, 8))
  val writeQueueWrapper = Module(new WriteQueueWrapper)

  // --- SIGNALS AND WIRES ---
  /** S-type modifier currently processing */
  val mod = Mux(io.id.ls === StypeLoadStore.LOAD, readQueue.io.deq.bits.mod, writeQueue.io.deq.bits.mod)
  /** Number of entries in the read queue */
  val count = readQueue.io.count

  // --- CONNECTIONS ---
  vec.io.in <> io.id.vec
  edof.io.in <> io.id.edof
  neighbour.io.in <> io.id.neighbour
  indexGen.io.in <> neighbour.io.indexGen
  val vecActive = mod === VEC
  val neighbourActive: Bool = mod === ELEM || mod === SEL || mod === FCN || mod === EDN1 || mod === EDN2
  val edofActive: Bool = mod === DOF || mod === FDOF

  addrGen.io.in <> vec.io.out
  when(edofActive) {
    addrGen.io.in <> edof.io.addrGen
  } .elsewhen(neighbourActive) {
    addrGen.io.in <> indexGen.io.addrGen
  }

  //Explicit handling of ready and valid signals
  //If count > 0, modules should only forward their data to addrGen if connected.
  // If nothing is processing, all modules have a ready input
  vec.io.out.ready := Mux(count > 0.U, Mux(vecActive, addrGen.io.in.ready, false.B), true.B)
  edof.io.addrGen.ready := Mux(count > 0.U, Mux(edofActive, addrGen.io.in.ready, false.B), true.B)
  indexGen.io.addrGen.ready := Mux(count > 0.U, Mux(neighbourActive, addrGen.io.in.ready, false.B), true.B)

  addrGen.io.mem <> mem.io.addrGen
  mem.io.wb <> wb.io.mem
  mem.io.writeQueue <> writeQueue.io.deq
  wb.io.readQueue <> readQueue.io.deq
  io.wb <> wb.io.id

  readQueue.io.enq <> io.id.readQueue
  io.id.writeQueue <> writeQueueWrapper.io.in
  writeQueueWrapper.io.out <> writeQueue.io.enq

  mem.io.we := io.id.ls === StypeLoadStore.STORE
  io.ctrl.rqCount := readQueue.io.count
  io.ctrl.wqCount := writeQueue.io.count
}

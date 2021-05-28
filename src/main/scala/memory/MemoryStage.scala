package memory

import chisel3._
import chisel3.util.{Decoupled, Queue, RegEnable, Valid}
import pipeline.StypeMod._
import pipeline.{IdMemIO, StypeLoadStore, StypeMod, WbIdIO}
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
}

/**
 * A wrapper module around the entirety of the memory stage. Connects to the [[pipeline.Decode]] stage for reads and writes.
 * Implements [[MemoryStageIO]]
 *
 * @param wordsPerBank The number of data words to store in *each* memory bank. The total amount of memory allotted
 *                     is NUM_MEMORY_BANKS*wordsPerBank, each of which is [[utils.Fixed.FIXED_WIDTH]] bits wide.
 * @param memInitFileLocation Location of memory initialization file. Each file must be named 'membank_x.txt', where 'x'
 *                            is a number [0;NUM_MEMORY_BANKS[. The parameter is the relative path to these files, to which the filename is appended.
 *                            If eg memInitFileLocation = "resources/meminit/", one file would be "resources/meminit/membank_0.txt"
 */
class MemoryStage(wordsPerBank: Int, memInitFileLocation: String) extends Module {
  val io = IO(new MemoryStageIO)

  // --- MODULES ---
  val addrGen = Module(new AddressGenerator())
  val mem = Module(new OnChipMemory(wordsPerBank, memInitFileLocation))
  val wb = Module(new MemoryWriteback)
  val edof = Module(new EdofGenerator)
  val vec = Module(new VecWrapper)

  val indexGen = Module(new IndexGenerator(pipe=true))
  val neighbour = Module(new NeighbourGenerator)

  val readQueue = Module(new Queue(new ReadQueueBundle, 8))
  val writeQueue = Module(new Queue(Vec(NUM_MEMORY_BANKS, SInt(FIXED_WIDTH.W)), 16))

  // --- REGISTERS ---


  // --- SIGNALS AND WIRES ---
  /** Most recent S-type modifier put into system */
  val mod = readQueue.io.deq.bits.mod
  /** Number of entries in the read queue */
  val count = readQueue.io.count

  // --- CONNECTIONS ---
  vec.io.in <> io.id.vec
  edof.io.in <> io.id.ijk
  neighbour.io.in <> io.id.ijk
  indexGen.io.in <> neighbour.io.indexGen
  //Neighbour and edof share a ready signal
  //As soon as we start processing, the ready signal is forwarded from the relevant module

  val vecActive = mod === VEC
  val neighbourActive: Bool = mod === ELEM || mod === SEL || mod === FCN || mod === EDN1 || mod === EDN2
  val edofActive: Bool = mod === DOF


  addrGen.io.in <> vec.io.out
  when(edofActive) {
    addrGen.io.in <> edof.io.addrGen
    io.id.ijk.ready := edof.io.in.ready
  } .elsewhen(neighbourActive) {
    addrGen.io.in <> indexGen.io.addrGen
    io.id.ijk.ready := neighbour.io.in.ready
  }

  //Explicit handling of ready and valid signals
  //If count > 0, only assert ready if mod is of our type. Otherwise set it to 0. If not processing, ready is true
  vec.io.out.ready := Mux(count > 0.U, Mux(vecActive, addrGen.io.in.ready, false.B), true.B)
  edof.io.addrGen.ready := Mux(count > 0.U, Mux(edofActive, addrGen.io.in.ready, false.B), true.B)
  indexGen.io.addrGen.ready := Mux(count > 0.U, Mux(neighbourActive, addrGen.io.in.ready, false.B), true.B)
  //Only assert valid into the edof/neighbour gen if the incoming tx matches their signature
  //Valid signal should depend on direct inputs from ijk gen.
  edof.io.in.valid := io.id.ijk.valid && io.id.readQueue.bits.mod === DOF
  val rdMod = io.id.readQueue.bits.mod
  neighbour.io.in.valid := io.id.ijk.valid && (rdMod === ELEM || rdMod === SEL || rdMod === FCN || rdMod === EDN1 || rdMod === EDN2)

  addrGen.io.mem <> mem.io.addrGen
  mem.io.wb <> wb.io.mem
  mem.io.writeQueue <> writeQueue.io.deq
  wb.io.readQueue <> readQueue.io.deq
  io.wb <> wb.io.id

  readQueue.io.enq <> io.id.readQueue
  writeQueue.io.enq <> io.id.wrData

  mem.io.we := io.id.ls === StypeLoadStore.STORE
}

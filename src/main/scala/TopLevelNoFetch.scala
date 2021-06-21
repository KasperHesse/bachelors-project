
import chisel3._
import memory.{MemWbIO, MemoryStage}
import execution._
import utils.{TimingOutput, TimingWrapper}

/**
 * The top level module for the topological optimizer WITHOUT a fetch stage.
 * This should allow us to synthesize this part -> save it as a design partition and reuse later -> faster design flow once compiled
 * @param wordsPerBank Number of elements in each memory bank
 * @param memInitFileLocation Location of memory initialization files. This is the relative path. Each memory bank initialization file must be named
 *                            'membank_x.txt', where x is in the range [0;NUM_MEMORY_BANKS[. If eg 'resources/meminit' is given as parameter, the first file should be named 'resources/meminit/membank_0.txt'
 */
class TopLevelNoFetch(wordsPerBank: Int, memInitFileLocation: String, clkFreq: Int) extends Module {
  val io = IO(new Bundle {
    val exout = new ExWbIO
    val fetch = Flipped(new IfIdIO)
    val fetchCtrl = Flipped(new IfControlIO)
    val timing = new TimingOutput(clkFreq)
  })


  val decode = Module(new Decode)
  val execute = Module(new Execute)
  val writeback = Module(new Writeback)
  val forward = Module(new Forwarding)
  val mem = Module(new MemoryStage(wordsPerBank, memInitFileLocation))
  val control = Module(new Control)
  val timing = Module(new TimingWrapper(clkFreq))

  io.fetch <> decode.io.fe
  decode.io.ex <> execute.io.id
  execute.io.wb <> writeback.io.ex
  writeback.io.id <> decode.io.wb
  writeback.io.fwd <> forward.io.wb
  forward.io.ex <> execute.io.fwd
  decode.io.mem <> mem.io.id
  decode.io.memWb <> mem.io.wb
  decode.io.time <> timing.io.id


  control.io.fe <> io.fetchCtrl
  control.io.id <> decode.io.ctrl
  control.io.ex <> execute.io.ctrl
  control.io.mem <> mem.io.ctrl

  io.exout <> execute.io.wb
  io.timing <> timing.io.out
}

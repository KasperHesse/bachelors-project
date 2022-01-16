import chisel3._
import execution._
import chisel3.experimental._
import chisel3.util.experimental.BoringUtils

class TopLevelTestWrapper(IMsize: Int, IMinitFileLocation: String, wordsPerBank: Int, memInitFileLocation: String) extends MultiIOModule {
//  val io = IO(new Bundle {
//    val idex = new IdExIO
//    val idctrl = new IdControlIO
//    val wbid = new WbIdIO
//    val idmem = new IdMemIO
//    val memid = new WbIdIO
//  })
  val top = Module(new TopLevelSim(IMsize, IMinitFileLocation, wordsPerBank, memInitFileLocation))
  val idex = IO(Output(top.decode.io.ex))
  val idctrl = IO(Output(top.decode.io.ctrl))

  //Connect to dontcares to suppress warnings - boringutils will drive the signals
//  io.idex := DontCare
//  io.idctrl := DontCare
//  io.wbid := DontCare
//  io.idmem := DontCare
//  io.memid := DontCare


  BoringUtils.bore(top.decode.io.ex, Seq(idex))
  BoringUtils.bore(top.decode.io.ctrl, Seq(idctrl))
//  BoringUtils.bore(top.writeback.io.id, Seq(io.wbid))
//  BoringUtils.bore(top.decode.io.mem, Seq(io.idmem))
//  BoringUtils.bore(top.mem.io.wb, Seq(io.memid))
}

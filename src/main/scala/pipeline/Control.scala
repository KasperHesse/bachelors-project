package pipeline

import chisel3._


class ControlIO extends Bundle {
  val id = Flipped(new IdControlIO)
  val ex = Flipped(new ExControlIO)
  val fe = Flipped(new IfControlIO)
}

class Control extends Module {
  val io = IO(new ControlIO)

  val stall = WireDefault(false.B)
  val stalls = WireDefault(VecInit(Seq.fill(4)(false.B)))

  val reg = RegInit(0.U(4.W))

  //When executing thread is about to execute an instruction which is different from the previous,
  //stall until currently executing operation is finished
  val execThread = io.id.threadCtrl(io.id.execThread)
  val memThread = io.id.threadCtrl(io.id.execThread + 1.U(1.W)) //Adding one should lap back to 0/inc to 1

  io.id.stall := false.B
  io.id.threadCtrl(0).stall := false.B
  io.id.threadCtrl(1).stall := false.B
  io.ex.stall := false.B
  io.fe.stall := false.B
  io.id.iload := false.B

  val iload = RegInit(false.B)




  //Assert instruction load when decode stage is idle and incoming instruction is istart
  val Oinst = io.fe.instr.asTypeOf(new OtypeInstruction)
  val isInstr: Bool = Oinst.iev === OtypeIEV.INSTR
  val isEnd = Oinst.se === OtypeSE.END
  private val isOtype: Bool = Oinst.fmt === InstructionFMT.OTYPE
  private val isSload: Bool = io.id.state === DecodeState.sLoad
  when((Oinst.se === OtypeSE.START && isInstr && isOtype && io.id.state === DecodeState.sIdle) || iload) {
    io.id.iload := true.B
    iload := true.B
  }
  when((isEnd && isInstr && isOtype && isSload)) {
    iload := false.B
    io.id.iload := false.B
  }

  //When instruction in execute stage does not match decoded instruction in execThread
  //stall execThread until destination queue is empty
  when(execThread.state === ThreadState.sExec && execThread.firstCycle && execThread.op =/= io.ex.op && io.ex.count =/= 0.U) {
    execThread.stall := true.B
    io.ex.stall := true.B
  }



//
//  when(io.id.state =/= DecodeOldStage.sIdle) {
//    //When executing or loading, if the currently executing operation is different from the decoded operation,
//    //stall until the currently executing operation is finished
//    //TODO: We can update this to not stall between add/sub operations
//
//    stalls(0) := (io.id.firstCycle && (io.ex.count =/= 0.U) && io.id.op =/= io.ex.op)
//
//    //When decoding XX instructions, stall for one clock cycle if not the exact same opcode
//    //This is to ensure that instructions properly pass into the destination queue
//    when(io.id.rtypemod === RtypeMod.XX && reg === 0.U && io.id.op =/= RegNext(io.id.op)) {
//      reg := 1.U
//      stalls(1) := true.B
//    } .elsewhen(io.id.rtypemod === RtypeMod.XX && reg === 1.U) {
//      reg := 0.U
//    }
//  }
//
//
//  stall := stalls.reduce( (a,b) => a || b) //Or reduction
//  io.id.stall := stall
//  io.ex.stall := stall
//  io.id.iload := true.B //TODO This is not the correct output

}


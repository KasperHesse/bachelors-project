package pipeline

import chisel3._

class Control extends Module {
  val io = IO(new ControlIO)

  val stall = WireDefault(false.B)
  val stalls = WireDefault(VecInit(Seq.fill(4)(false.B)))

  val reg = RegInit(0.U(4.W))

  when(io.id.state =/= DecodeOldStage.sIdle) {
    //When executing or loading, if the currently executing operation is different from the decoded operation,
    //stall until the currently executing operation is finished
    //TODO: We can update this to not stall between add/sub operations
    stalls(0) := (io.id.firstCycle && (io.ex.count =/= 0.U) && io.id.op =/= io.ex.op)

    //When decoding XX instructions, stall for one clock cycle if not the exact same opcode
    //This is to ensure that instructions properly pass into the destination queue
    when(io.id.rtypemod === RtypeMod.XX && reg === 0.U && io.id.op =/= RegNext(io.id.op)) {
      reg := 1.U
      stalls(1) := true.B
    } .elsewhen(io.id.rtypemod === RtypeMod.XX && reg === 1.U) {
      reg := 0.U
    }
  }


  stall := stalls.reduce( (a,b) => a || b) //Or reduction
  io.id.stall := stall
  io.ex.stall := stall
  io.id.iload := true.B //TODO This is not the correct output

}

class ControlIO extends Bundle {
  val id = Flipped(new IdControlOldIO)
  val ex = Flipped(new ExControlIO)
}
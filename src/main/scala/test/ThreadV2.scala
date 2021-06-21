package test

import chisel3._
import chisel3.util.log2Ceil
import execution.{InlineVectorRegisterFile, RtypeInstruction, ThreadIO, ThreadState}
import execution.ThreadState.sIdle
import utils.Config._
import utils.Fixed._

/**
 * Another version of the thread module, made to try and work around stupid uninferred logic
 * @param id
 */
class ThreadV2(id: Int) extends Module {
  require(id == 0 || id == 1, "Thread ID must be 0 or 1")
  val io = IO(new ThreadIO)

  // --- MODULES ---
  val vRegFile = new InlineVectorRegisterFile(width=NUM_VREG, depth=VREG_DEPTH, s"src/test/resources/meminit/vreg$id.txt")

  // --- REGISTERS ---
  // --- SIGNALS AND WIRES
  val Rinst = io.instr.asTypeOf(new RtypeInstruction)
  val X = RegInit(0.U(4.W))
  val Y = RegInit(0.U(4.W))
  val Xtick = X === 3.U
  val Ytick = Y === 4.U

  X := Mux(Xtick, 0.U, X + 1.U)
  Y := Mux(Xtick, Mux(Ytick, 0.U, Y + 1.U), Y)

  val v_rs1 = (Rinst.rs1 << 1).asUInt() + Y
  val v_rs2 = (Rinst.rs2 << 1).asUInt + Y
  val rdData1 = vRegFile.setReadPort(v_rs1)
  val rdData2 = vRegFile.setReadPort(v_rs2)
  vRegFile.setWritePort(io.wb.rd.reg, io.wb.wrData, io.wb.we)

  val a_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  val b_subvec = Wire(Vec(SUBVECTORS_PER_VREG, Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until SUBVECTORS_PER_VREG) {
    a_subvec(i) := rdData1.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
    b_subvec(i) := rdData2.slice(i*NUM_PROCELEM, (i+1)*NUM_PROCELEM)
  }

  io.sRegFile := DontCare
  io.ex := DontCare
  io.ctrl := DontCare
  io.mem := DontCare
  io.threadOut := DontCare

  io.ex.a := a_subvec(X)
  io.ex.b := b_subvec(X)

  io.ip := 0.U
  io.stateOutUint := 0.U
  io.threadOut.state := ThreadState.sEstart
}

class VregFile(width: Int, depth: Int) {
  val mem = SyncReadMem(width, SInt((depth*FIXED_WIDTH).W))

  def setReadPort(rs: UInt): Vec[SInt] = {
    val rdData = mem.read(rs)
    val rdDataVec = Wire(Vec(depth, SInt(FIXED_WIDTH.W)))
    for(i <- 0 until depth) {
      rdDataVec(i) := rdData((i+1)*FIXED_WIDTH-1, i*FIXED_WIDTH).asSInt()
    }
    rdDataVec
  }
  def setWritePort(rd: UInt, wrData: Vec[SInt]): Unit = {
    mem.write(rd, wrData.asUInt().asSInt())
  }
}
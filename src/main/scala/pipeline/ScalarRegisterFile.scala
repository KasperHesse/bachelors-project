package pipeline

import chisel3._
import chisel3.util._
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import utils.Fixed.double2fixed

/**
 * A scalar register file. Register at index 0 is always tied to the value 0. Implements [[ScalarRegFileIO]].
 */
class ScalarRegisterFile extends Module {
  val io = IO(new ScalarRegFileIO)

  var regFile: Vec[SInt] = RegInit(VecInit(Seq.fill(NUM_SCALAR_REGISTERS)(0.S(FIXED_WIDTH.W))))
  val arr = for(i <- 0 until NUM_SCALAR_REGISTERS) yield {
    double2fixed(i).S(FIXED_WIDTH.W)
  }

  if(SIMULATION) {
//    val inits: Seq[SInt] = for(i <- 0 until NUM_SCALAR_REGISTERS) yield {
//      double2fixed(i).S(FIXED_WIDTH.W)
//    }
    regFile = RegInit(VecInit(arr))
  }

  when(io.we && io.rd =/= 0.U) {
    regFile(io.rd) := io.wrData
  }
  io.rdData1 := regFile(io.rs1)
  io.rdData2 := regFile(io.rs2)
}

class ScalarRegFileIO extends Bundle {
  /** Write enable bit */
  val we = Input(Bool())
  /** Register select 1 */
  val rs1 = Input(UInt(log2Ceil(NUM_SCALAR_REGISTERS).W))
  /** Register select 2 */
  val rs2 = Input(UInt(log2Ceil(NUM_SCALAR_REGISTERS).W))
  /** Destination register for write */
  val rd = Input(UInt(log2Ceil(NUM_SCALAR_REGISTERS).W))
  /** Write data */
  val wrData = Input(SInt(FIXED_WIDTH.W))
  /** Read data 1, read from register specified by rs1 */
  val rdData1 = Output(SInt(FIXED_WIDTH.W))
  /** Read data 2, read from register specified by rs2 */
  val rdData2 = Output(SInt(FIXED_WIDTH.W))
}

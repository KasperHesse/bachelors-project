package test

import chisel3._
import chisel3.util._
import utils.Fixed.FIXED_WIDTH

/**
 * Dual ported RAM using SyncReadMem. To have dual ported RAM, we must have separate read and write address lines
 * Using these, we can read/write at the same time to our memory chip.
 * If wrAddr == rdAddr, we will read the value being stored instead of the currently stored value (looks like)
 */
abstract class MemTest extends Module {
  val io = IO(new Bundle {
    val wrAddr = Input(UInt(10.W))
    val wrData = Input(UInt(16.W))
    val wrEnable = Input(Bool())
    val rdAddr = Input(UInt(10.W))
    val rdData = Output(UInt(16.W))
    val rdEnable = Input(Bool())
  })

}

/**
 * Using SyncReadMem maps to block memory
 * Uses 15 ALM's and 16kB of block memory, out of a total 12MB
 */
class SyncReadMemTest extends Module {
  val io = IO(new Bundle {
    val wrAddr = Input(UInt(6.W))
    val wrData = Input(Vec(4, SInt(8.W)))
    val wrEnable = Input(Bool())
    val wrMask = Input(UInt(16.W))

    val rdAddr = Input(UInt(6.W))
    val rdData = Output(Vec(4, SInt(8.W)))
    val rdMask = Input(UInt(16.W))
    val rdEnable = Input(Bool())
  })
  val mem = SyncReadMem(64, Vec(16, Vec(4, SInt(8.W))))

  //Which of these is the correct way to create a vector of zeros with one of the elements set, I wonder?
  val wrData = VecInit(Seq.fill(16)(VecInit(Seq.fill(4)(0.S(8.W)))))
    wrData(io.wrMask.asUInt) := io.wrData

  when(io.wrEnable) {
    mem.write(io.wrAddr, wrData, io.wrMask.asBools())
  }
  io.rdData := mem.read(io.rdAddr, io.rdEnable)(io.rdMask)
}

/**
 * This way of creating dual-ported RAM doesn't work, and instead instantiates registers and not block RAM.
 */
class SyncReadMemDualPort extends Module {
  val io = IO(new Bundle {
    val wrAddr1 = Input(UInt(10.W))
    val wrData1 = Input(UInt(16.W))
    val wrEnable1 = Input(Bool())
    val rdAddr1 = Input(UInt(10.W))
    val rdData1 = Output(UInt(16.W))
    val rdEnable1 = Input(Bool())
    val wrAddr2 = Input(UInt(10.W))
    val wrData2 = Input(UInt(16.W))
    val wrEnable2 = Input(Bool())
    val rdAddr2 = Input(UInt(10.W))
    val rdData2 = Output(UInt(16.W))
    val rdEnable2 = Input(Bool())
  })
  val mem = SyncReadMem(1024, UInt(16.W))
  when(io.wrEnable1) {
    mem.write(io.wrAddr1, io.wrData1)
  }
  io.rdData1 := mem.read(io.rdAddr1, io.rdEnable1)

  when(io.wrEnable2) {
    mem.write(io.wrAddr2, io.wrData2)
  }
  io.rdData2 := mem.read(io.rdAddr2, io.rdEnable2)
}

/**
 * Looks as if it maps to registers
 * The 1024 elements x 16 bits maps to 16k registers / 8k ALMs, 7% of total ALM's.
 * No block memory bits were used
 */
class MemMemTest extends MemTest {
  val mem = Mem(1024, UInt(16.W))
  when(io.wrEnable) {
    mem.write(io.wrAddr, io.wrData)
  }
  io.rdData := mem.read(io.rdAddr)
}

object MemTest {
  def apply():  SyncReadMemTest = {
    new SyncReadMemTest
  }
}


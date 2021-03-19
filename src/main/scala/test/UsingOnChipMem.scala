package test

import chisel3._

class UsingOnChipMem extends Module {
  val io = IO(new Bundle {
    val en = Input(Bool())
    val we = Input(Bool())
    val mask = Input(UInt(1.W))
    val rs1 = Input(UInt(5.W))
    val rd = Output(SInt(54.W))
  })


  //We want to try and use the on-chip memory and the register file to move values back and forth. Let's see how that goes
  val mem = Module(new memory.OnChipMemory(numBanks = 24, wordsPerBank = 1024))
  val regFile = Module(new pipeline.VectorRegisterFile(32, 24, 24))

  //Generate a random value. Use that to read from reg file, and write that into memory.
  //At the same time, read from another random address in memory and write that into reg file

  val regAddr = RegInit(0.U(5.W))
  regAddr := regAddr + 1.U

  val memAddr = RegInit(0.U(10.W))
  memAddr := memAddr + 1.U

  for(i <- 0 until 24) {
    mem.io.in.rdAddr(i) := memAddr + i.U
  }
  mem.io.in.en := io.en
  mem.io.in.we := io.we
  mem.io.in.wrData := regFile.io.rdData1
  for(i <- 0 until 24) {
    mem.io.in.wrAddr(i) := (regAddr << 2.U).asUInt() + i.U
  }

  regFile.io.wrData := mem.io.out.rdData
  regFile.io.rd := memAddr(4,0)
  regFile.io.we := io.we
  regFile.io.wrMask := io.mask
  regFile.io.rdMask1 := io.mask
  regFile.io.rs1 := io.rs1

  regFile.io.rs2 := 0.U
  regFile.io.rdMask2 := 0.U

  io.rd := regFile.io.rdData1(io.rs1)

}

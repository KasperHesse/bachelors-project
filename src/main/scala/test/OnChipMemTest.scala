package test

import chisel3._
import chisel3.util.Counter

class OnChipMemTestIO[T <: Data](dataType: T) extends Bundle {
  val rdAddr1 = Input(UInt(4.W))
  val rdAddr2 = Input(UInt(4.W))
  val wrAddr = Input(UInt(4.W))
  val wrData = Input(Vec(3, dataType))
  val rdData1 = Output(Vec(3, dataType))
  val rdData2 = Output(Vec(3, dataType))
}
/**
 * This module generates verilog which synthesizes to on-chip memory
 */
class OnChipMemTest[T <: Data](onchip: Boolean = false, dataType: T = SInt(8.W)) extends Module {
  override val desiredName = s"OnChipMemTest${onchip.toString.toUpperCase}"
//  import test.OCRegFile
  val io = IO(new OnChipMemTestIO(dataType))




  val X = if(onchip) RegInit(0.U) else RegInit(0.U(4.W)) //Note the lack/addition of bitwidth
  val Y = if(onchip) RegInit(0.U) else RegInit(0.U(4.W))

  val Xtick = X === 3.U
  val Ytick = Y === 4.U
  X := Mux(Xtick, 0.U, X + 1.U)
  Y := Mux(Xtick, Mux(Ytick, 0.U, Y + 1.U), Y)

  val rdAddr1 = ((io.rdAddr1 << 1).asUInt + Y)(3,1)
  val rdAddr2 = ((io.rdAddr2 << 1).asUInt + Y)(3,1)

  //V1: Module which acts as a wrapper around the register file. Does not work (but sometimes it does?)
  val mem = Module(new OCRegFile(dataType))
  mem.io.rs1 := rdAddr1
  mem.io.rs2 := rdAddr2
  io.rdData1 := mem.io.rdData1
  io.rdData2 := mem.io.rdData2
  mem.io.wrData := io.wrData
  mem.io.rd := io.wrAddr

  //V2: Using a Scala class to generate the register file directly in this module. Should work
//  val mem = new RegFile(dataType, vecSize=3, entries=8)
//  io.rdData1 := mem.setReadPort1(rdAddr1)
//  io.rdData2 := mem.setReadPort2(rdAddr2)
//  mem.setWritePort(io.wrAddr, io.wrData)

  //V3: Directly creating the register file in this module. Should work
//  val mem = SyncReadMem(8, Vec(3, dataType))
//  io.rdData1 := mem.read(rdAddr1)
//  io.rdData2 := mem.read(rdAddr2)
//  mem.write(io.wrAddr, io.wrData)
}

class OCRegFile[T <: Data](dataType: T) extends Module {
  val io = IO(new Bundle {
    val rdData1 = Output(Vec(3, dataType))
    val rdData2 = Output(Vec(3, dataType))
    val wrData = Input(Vec(3, dataType))
    val rs1 = Input(UInt(4.W))
    val rs2 = Input(UInt(4.W))
    val rd = Input(UInt(4.W))
  })

  val regFile = SyncReadMem(8, Vec(3, dataType))
  regFile.write(io.rd, io.wrData)
  io.rdData1 := regFile.read(io.rs1)
  io.rdData2 := regFile.read(io.rs2)
}

class RegFile[T <: Data](dataType: T, vecSize: Int, entries: Int) {
  val mem = SyncReadMem(entries, Vec(vecSize, dataType))

  def setReadPort1(rs1: UInt): Vec[T] = {
    mem.read(rs1)
  }

  def setReadPort2(rs2: UInt): Vec[T] = {
    mem.read(rs2)
  }

  def setWritePort(rd: UInt, wrData: Vec[T]): Unit = {
    mem.write(rd, wrData)
  }
}

class UsingOnChipMemTest[T <: Data](dataType: T = SInt(8.W)) extends Module {
  val io = IO(new Bundle {
    val mod1 = new OnChipMemTestIO(dataType)
    val mod2 = new OnChipMemTestIO(dataType)
  })

  val mod1 = Module(new OnChipMemTest(false))
  val mod2 = Module(new OnChipMemTest(true))

  io.mod1 <> mod1.io
  io.mod2 <> mod2.io
}


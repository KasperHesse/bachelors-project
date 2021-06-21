package utils

import chisel3._
import chisel3.util.{RegEnable, log2Ceil}
import Config.SIMULATION
import execution.IdTimingIO

/**
 * A module used to keep track of time.
 */
class TimingModule(clkFreq: Int) extends Module {
  val io = IO(new Bundle {
    /** Enable/disable signal. If enabled, timing is activated. If disabled, times are kept constant  */
    val en = Input(Bool())
    /** Clear signal. When toggled, all registers are reset */
    val clr = Input(Bool())
    /** Milli-second value */
    val ms = Output(UInt(log2Ceil(1000).W))
    /** Seconds value */
    val s = Output(UInt(log2Ceil(60).W))
    /** Minutes value */
    val m = Output(UInt(log2Ceil(240).W))

//    val msGround = Output(UInt(log2Ceil(1000).W))
//    val sGround = Output(UInt(log2Ceil(1000).W))
//    val mGround = Output(UInt(log2Ceil(1000).W))
//
//    val blink = Output(Bool())
//
//    val clrPassthrough = Output(Bool())
//    val enPassthrough = Output(Bool())
  })

  val blinkReg = RegInit(false.B)

  val cntReg = RegInit(0.U(log2Ceil(clkFreq/1000+1).W))
  val msReg = RegInit(0.U(log2Ceil(1000).W))
  val sReg = RegInit(0.U(log2Ceil(60).W))
  val mReg = RegInit(0.U(log2Ceil(240).W))

  val cntTick = cntReg === (clkFreq / 1000 - 1).U
  val msTick =  if(SIMULATION) msReg === 5.U else msReg === 1000.U
  val sTick =   if(SIMULATION) sReg === 4.U else sReg === 60.U

  when(io.en) {
    cntReg := Mux(cntTick, 0.U, cntReg + 1.U)
    msReg := Mux(msTick && cntTick, 0.U, Mux(cntTick, msReg + 1.U, msReg))
    sReg := Mux(sTick && msTick && cntTick, 0.U, Mux(msTick && cntTick, sReg + 1.U, sReg))
    mReg := Mux(sTick && msTick && cntTick, mReg + 1.U, mReg)
  }
  when(io.clr) {
    cntReg := 0.U
    msReg := 0.U
    sReg := 0.U
    mReg := 0.U
  }
  io.ms := msReg
  io.s := sReg
  io.m := mReg

//  io.msGround := 0.U
//  io.sGround := 0.U
//  io.mGround := 0.U
//
//  io.clrPassthrough := io.clr
//  io.enPassthrough := io.en
//
//  blinkReg := Mux(cntTick && msTick, !blinkReg, blinkReg)
//  io.blink := blinkReg
}

/**
 * Output ports for the timing module to board and HSMC pins
 * @param clkFreq Clock frequency being used in the design
 */
class TimingOutput(val clkFreq: Int) extends Bundle {
  //log2Ceil(240) is an arbitrary limit
  /** Milli-second value */
  val ms = Output(UInt(log2Ceil(1000).W))
  /** Seconds value */
  val s = Output(UInt(log2Ceil(60).W))
  /** Minutes value */
  val m = Output(UInt(8.W))
  /** Ground signal for the millisecond values */
  val msGround = Output(UInt(log2Ceil(1000).W))
  /** Ground signal for the seconds values */
  val sGround = Output(UInt(log2Ceil(60).W))
}

/**
 * I/O ports for [[TimingWrapper]]
 * @param clkFreq
 */
class TimingWrapperIO(val clkFreq: Int) extends Bundle{
  val id = Input(new IdTimingIO)
  val out = new TimingOutput(clkFreq)
}

/**
 * A wrapper around [[TimingModule]], for the additional I/O ports necessary to display on the HSMC breakout board.
 * Implements [[TimingWrapperIO]]
 * @param clkFreq The clock frequency that the design is operating at
 */
class TimingWrapper(clkFreq: Int) extends Module {
  val io = IO(new TimingWrapperIO(clkFreq))

  val timing = Module(new TimingModule(clkFreq))

  timing.io.en := io.id.en
  timing.io.clr := io.id.clr
  io.out.ms := timing.io.ms
  io.out.s := timing.io.s
  io.out.m := timing.io.m
  io.out.sGround := 0.U
  io.out.msGround := 0.U
}
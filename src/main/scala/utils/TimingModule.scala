package utils

import chisel3._
import chisel3.util.{RegEnable, log2Ceil}
import Config.SIMULATION
import execution.TimingWrapperIO

/**
 * A module used to keep track of execution time.
 */
class TimingModule(clkFreq: Int) extends Module {
  val io = IO(new Bundle {
    /** Enable/disable signal. If enabled, timing is activated. If disabled, times values are kept constant  */
    val en = Input(Bool())
    /** Clear signal. When toggled, all registers are reset */
    val clr = Input(Bool())
    /** Milli-second value */
    val ms = Output(UInt(log2Ceil(1000).W))
    /** Seconds value */
    val s = Output(UInt(log2Ceil(60).W))
    /** Minutes value */
    val m = Output(UInt(log2Ceil(240).W))
    /** Blinking LED to show that the accelerator has been transferred correctly */
    val blink = Output(Bool())
  })

  //The LED should blink with a frequency of 1Hz
  val blinkCntReg = RegInit(0.U(log2Ceil(clkFreq+1).W))
  val blinkReg = RegInit(false.B)
  val blinkTick = blinkCntReg === clkFreq.U
  blinkCntReg := Mux(blinkTick, 0.U, blinkCntReg + 1.U)
  blinkReg := Mux(blinkTick, !blinkReg, blinkReg)


  //Registers for counting ms, seconds and minutes
  val cntReg = RegInit(0.U(log2Ceil(clkFreq/1000+1).W))
  val msReg = RegInit(0.U(log2Ceil(1000).W))
  val sReg = RegInit(0.U(log2Ceil(60).W))
  val mReg = RegInit(0.U(8.W))


  val cntTick = cntReg === (clkFreq / 1000 - 1).U
  val msTick =  if(SIMULATION) msReg === 5.U else msReg === 999.U
  val sTick =   if(SIMULATION) sReg === 4.U else sReg === 59.U

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
  io.blink := blinkReg
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
  io.out.blink := timing.io.blink

}
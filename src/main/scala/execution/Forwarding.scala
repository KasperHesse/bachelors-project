package pipeline

import chisel3._
import utils.Config._
import utils.Fixed._
import chisel3.util._

/**
 * I/O ports for the [[Forwarding]] module.
 */
class ForwardingIO extends Bundle {
  val ex = Flipped(new ExFwdIO)
  val wb = Flipped(new WbFwdIO)
}

/**
 * Forwarding module used to avoid data hazards. Implements [[ForwardingIO]]
 */
class Forwarding extends Module {
  val io = IO(new ForwardingIO)

  //Generate bools indicating if any of the destinations match the rs1 value. Don't forward if rd = s0
  val rs1swaps = Wire(Vec(VREG_DEPTH/NUM_PROCELEM, Bool()))
  for(i <- 0 until io.wb.rd.length) {
    val rd = io.wb.rd(i)
    rs1swaps(i) := (io.ex.rs1.rf === rd.rf && io.ex.rs1.reg === rd.reg && io.ex.rs1.subvec === rd.subvec && io.wb.rdValids(i) && !(rd.reg === 0.U && rd.rf === RegisterFileType.SREG))
  }

  //Boil it down to one bool
  val rs1swap = rs1swaps.reduce((a,b) => a|b)

  //Generate a select signal for choosing the corresponding output
  val rs1sel = OHToUInt(rs1swaps.asUInt())

  //Set that output
  io.ex.rs1newData := io.wb.wbData(rs1sel)

  //Do it again
  val rs2swaps = Wire(Vec(VREG_DEPTH/NUM_PROCELEM, Bool()))
  for(i <- 0 until io.wb.rd.length) {
    val rd = io.wb.rd(i)
    rs2swaps(i) := (io.ex.rs2.rf === rd.rf && io.ex.rs2.reg === rd.reg && io.ex.rs2.subvec === rd.subvec && io.wb.rdValids(i))
  }
  val rs2swap = rs2swaps.reduce((a,b) => a|b)
  val rs2sel = OHToUInt(rs2swaps.asUInt())
  io.ex.rs2newData := io.wb.wbData(rs2sel)

  io.ex.rs2swap := rs2swap
  io.ex.rs1swap := rs1swap
}

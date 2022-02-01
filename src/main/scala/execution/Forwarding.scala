package execution

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
    //If register file type and subvec match, and the output from writeback is valid, we should swap
    rs1swaps(i) := (io.ex.rs1.rf === rd.rf && io.ex.rs1.reg === rd.reg && io.ex.rs1.subvec === rd.subvec && io.wb.rdValids(i) && !(rd.reg === 0.U && rd.rf === RegisterFileType.SREG))
  }

  //Boil it down to one bool indicating if a swap should be performed
  val rs1swap = rs1swaps.reduce((a,b) => a|b)

  //Generate a select signal for choosing the corresponding output
  val rs1sel = OHToUInt(rs1swaps.asUInt)


  //Do it again
  val rs2swaps = Wire(Vec(VREG_DEPTH/NUM_PROCELEM, Bool()))
  for(i <- 0 until io.wb.rd.length) {
    val rd = io.wb.rd(i)
    rs2swaps(i) := (io.ex.rs2.rf === rd.rf && io.ex.rs2.reg === rd.reg && io.ex.rs2.subvec === rd.subvec && io.wb.rdValids(i))
  }
  val rs2swap = rs2swaps.reduce((a,b) => a|b)
  val rs2sel = OHToUInt(rs2swaps.asUInt)


  //Set output values
  //WHen forwarding to a .xv-instruction, only the value at position (0) should be forwarded
  when(io.ex.rs1.rf === RegisterFileType.XREG && io.ex.rs2.rf === RegisterFileType.VREG) {
    for(i <- 0 until NUM_PROCELEM) {
      io.ex.rs1newData(i) := io.wb.wbData(rs1sel)(0)
      io.ex.rs2newData(i) := io.wb.wbData(rs2sel)(0)
    }
  } .otherwise {
    io.ex.rs1newData := io.wb.wbData(rs1sel)
    io.ex.rs2newData := io.wb.wbData(rs2sel)
  }

  io.ex.rs1swap := rs1swap
  io.ex.rs2swap := rs2swap

}

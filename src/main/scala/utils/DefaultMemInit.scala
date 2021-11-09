package utils

import chisel3.SInt
import utils.Config._
import utils.Fixed._

object DefaultMemInit extends App {

  //Create vreg
  def initVreg(id: Int): Unit = {
    for(d <- 0 until VREG_DEPTH) {
      val mif = s"src/resources/meminit_test/vreg${id}_$d.hex.txt"
      val contents = Array.ofDim[Long](NUM_VREG)
      for(w <- 0 until NUM_VREG) {
        contents(w) = double2fixed(w*VREG_DEPTH+d)
      }
      utils.Assembler.writeMemInitFile(mif, contents, 16)
    }
  }

  def initXreg(id: Int): Unit = {
    for(d <- 0 until XREG_DEPTH) {
      val mif = s"src/resources/meminit_test/xreg${id}_$d.hex.txt"
      val contents = Array.ofDim[Long](NUM_XREG)
      for(w <- 0 until NUM_XREG) {
        contents(w) = double2fixed(w*XREG_DEPTH+d)
      }
      utils.Assembler.writeMemInitFile(mif, contents, 16)
    }
  }

  def initSreg(): Unit = {
    val contents = Array.ofDim[Long](NUM_SREG)
    for(v <- 0 until NUM_SREG) {
      contents(v) = double2fixed(v)
    }
    utils.Assembler.writeMemInitFile("src/resources/meminit_test/sreg.hex.txt", contents, 16)
  }

  initVreg(0)
  initVreg(1)
  initXreg(0)
  initXreg(1)
  initSreg()
}

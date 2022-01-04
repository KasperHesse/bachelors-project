package utils

import chisel3.SInt
import utils.Config._
import utils.Fixed._

object DefaultMemInit extends App {

  //Create vreg
  def initVreg(id: Int): Unit = {
    for(d <- 0 until VREG_DEPTH) {
      val mif = s"src/resources/meminit_default/vreg${id}_$d.txt"
      val contents = Array.ofDim[Long](NUM_VREG)
      for(w <- 0 until NUM_VREG) {
        contents(w) = double2fixed(w*VREG_DEPTH+d)
      }
      utils.Assembler.writeMemInitFile(mif, contents, 16)
    }
  }

  def initXreg(id: Int): Unit = {
    for(d <- 0 until XREG_DEPTH) {
      val mif = s"src/resources/meminit_default/xreg${id}_$d.txt"
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
    utils.Assembler.writeMemInitFile("src/resources/meminit_default/sreg.txt", contents, 16)
  }

  def initMem(): Unit = {
    for(b <- 0 until NUM_MEMORY_BANKS) {
      val mif = s"src/resources/meminit_default/membank_$b.txt"
      val contents = Array.ofDim[Long](WORDS_PER_BANK)
      for(w <- 0 until WORDS_PER_BANK) {
        contents(w) = double2fixed(w*NUM_MEMORY_BANKS+b)
      }
      utils.Assembler.writeMemInitFile(mif, contents, 16)
    }
  }

  def apply(): Unit = {
    initVreg(0)
    initVreg(1)
    initXreg(0)
    initXreg(1)
    initSreg()
    initMem()
  }


  apply()

}

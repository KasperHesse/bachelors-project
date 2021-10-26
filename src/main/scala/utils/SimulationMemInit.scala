package utils

import execution.StypeBaseAddress
import memory.AddressDecode
import utils.Assembler.writeMemInitFile
import utils.Config._
import utils.Fixed.{FIXED_WIDTH, double2fixed}
import utils.LitVals._

import scala.io.Source
/**
 * Helper object used to initialize memory based on a memory dump when simulating.
 * This allows us to pick up simulation in the middle of a function, reducing the overhead for running further tests
 */
object SimulationMemInit extends App {

  def apply(testName: String, hash: String, memInitFileLocation: String): Int = {
    val numWords = 8*NDOFSIZE + 5*NELEMSIZE
    val wordsPerBank = numWords/8
    val mem = Array.ofDim[Double](8, wordsPerBank)
    val vreg = Array.ofDim[Double](2, NUM_VREG, VREG_DEPTH)
    val xreg = Array.ofDim[Double](2, NUM_XREG, XREG_DEPTH)
    val sreg = Array.ofDim[Double](NUM_SREG)

    def readMemFile(vec: String, baseAddrIndex: Int): Unit = {
      val file = Source.fromFile(s"memdump/$testName/$hash/mem_$vec.csv")

      val lines = file.getLines().toList.tail
      val baseAddress = AddressDecode.mapping(baseAddrIndex)

      for(line <- lines) {
        val values = line.split(',')
        val index = values(3).toInt
        val value = values.last.toDouble
        val addr = baseAddress + index

        val bank = addr % NUM_MEMORY_BANKS
        val idx = addr / NUM_MEMORY_BANKS
        mem(bank)(idx) = value
      }
      file.close()
    }

    def readVregFile(id: Int): Unit = {
      val file = Source.fromFile(s"memdump/$testName/$hash/vreg_$id.csv")

      val lines = file.getLines().toList.tail
      for(reg <- lines.indices) {
        val values = lines(reg).split(',').toList.tail
        for(j <- values.indices) {
          vreg(id)(reg)(j) = values(j).toDouble
        }
      }
      file.close()
    }

    def readXregFile(id: Int): Unit = {
      val file = Source.fromFile(s"memdump/$testName/$hash/xreg_$id.csv")

      val lines = file.getLines().toList.tail
      for(reg <- lines.indices) {
        val values = lines(reg).split(',').toList.tail
        for(j <- values.indices) {
          xreg(id)(reg)(j) = values(j).toDouble
        }
      }
      file.close()
    }

    def readSregFile(): Unit = {
      val file = Source.fromFile(s"memdump/$testName/$hash/sreg_0.csv")

      val values = file.getLines().toList.last.split(',').tail
      for(j <- values.indices) {
        sreg(j) = values(j).toDouble
      }
      file.close()
    }

    readMemFile("X", X)
    readMemFile("XNEW", XNEW)
    readMemFile("XPHYS", XPHYS)
    readMemFile("DC", DC)
    readMemFile("DV", DV)
    readMemFile("F", F)
    readMemFile("U", U)
    readMemFile("R", R)
    readMemFile("Z", Z)
    readMemFile("P", P)
    readMemFile("Q", Q)
    readMemFile("INVD", INVD)
    readMemFile("TMP", TMP)

    //Write mem init files
    for(i <- 0 until NUM_MEMORY_BANKS) {
      val memFile = s"$memInitFileLocation/membank_$i.txt"
      val contents = mem(i).map(double2fixed).map(c => c & ((1L << FIXED_WIDTH)-1)) //Mask to preserve only FIXED_WIDTH lower bits
      writeMemInitFile(memFile, contents, 16)
    }

    //Read registers and write reginit files
    readVregFile(0)
    readVregFile(1)
    readXregFile(0)
    readXregFile(1)
    readSregFile()


    wordsPerBank
  }

  apply("simple", "fcc1ef", "meminit")
}

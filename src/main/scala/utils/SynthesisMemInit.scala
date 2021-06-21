package utils

import memory.AddressDecode.mapping
import execution.StypeBaseAddress._
import utils.Assembler.writeMemInitFile
import utils.Config._
import utils.Fixed._

import scala.collection.mutable.ListBuffer
/**
 * A helper object to create the memory initialization files used for synthesis
 */
object SynthesisMemInit {

  /**
   * Creates memory init files for synthesis.
   * @param memInitFileLocation The base location of the memory initialization files to be created. Should be a directory
   * @return The number of words in each memory bank
   */
  def apply(memInitFileLocation: String): Int = {

    //total number of memory words required
    val numWords = 8*NDOFSIZE+5*NELEMSIZE
    val wordsPerBank = numWords/8

    val fOffset = mapping(F.litValue.toInt)
    val indexList = ListBuffer.empty[Int]

    //For each value of ny with i=nelx-1 and k=0, calculate the bank indices.
    //We take the bank indices at (16) and (18) to calculate the indices for z-direction DOF's in bank 0 and 2 (banks at i=nelx-1 and k=0)
    val i = NELX-1
    val k = 0
    for(j <- 0 until NELY) {
      val edof = getEdof(i,j,k)
      //We're only interested in the DOF's in z-direction, entries 16 and 18
      indexList += edof(16)
      if(j != NELY-1) indexList += edof(18) //On the final iteration, the index for bank 2 is outside of the grid since grid size is always even
    }
    val indices = indexList.distinct
    //Set these values in memory
    val mem = Array.ofDim[Double](8,wordsPerBank)
    val addresses = indices.map(_+fOffset)
    for(addr <- addresses) {
      val bank = addr & 0x7
      val offset = addr/8
      mem(bank)(offset) = -1
    }

    //Create memory initialization files
    for(i <- 0 until NUM_MEMORY_BANKS) {
      val memFile = s"$memInitFileLocation/membank_$i.txt"
      val contents = mem(i).map(double2fixed).map(c => c & ((1L << FIXED_WIDTH)-1)) //Mask to preserve only FIXED_WIDTH lower bits
      writeMemInitFile(memFile, contents, 16)
    }

    wordsPerBank
  }



  /**
   * Compute the indices of the of the 24 degrees of freedom associated with the 8 corners of the element at (i,j,k) in the grid.
   *
   * @param i Current iteration/coordinate in the x-direction
   * @param j Current iteration/coordinate in the y-direction
   * @param k Current iteration/coordinate in the z-direction
   * @return An array of 24 integers holding the 24 DOF's of the specified element
   */
  def getEdof(i: Int, j: Int, k: Int): Array[Int] = {
    require(0 <= i && i < NELX, "i must be in the range [0;NELX[")
    require(0 <= j && j < NELY, "j must be in the range [0;NELY[")
    require(0 <= k && k < NELZ, "k must be in the range [0;NELZ[")
    val NYL = NY/2
    val NZL = NZ/2
    val NYH = (NY+1)/2
    val NZH = (NZ+1)/2

    val nx0 = i/2
    val nx1 = i/2 + (i & 1)
    val ny0 = j/2
    val ny1 = j/2 + (j & 1)
    val nz0 = k/2
    val nz1 = k/2 + (k & 1)

    //Calculate nIndices
    val nIndex = Array.ofDim[Int](8)
    val edof = Array.ofDim[Int](24)

    nIndex(0) = nx1 * NYH * NZH + nz1 * NYH + ny1
    nIndex(1) = nx1 * NYL * NZH + nz1 * NYL + ny0
    nIndex(2) = nx1 * NYH * NZL + nz0 * NYH + ny1
    nIndex(3) = nx1 * NYL * NZL + nz0 * NYL + ny0
    nIndex(4) = nx0 * NYH * NZH + nz1 * NYH + ny1
    nIndex(5) = nx0 * NYL * NZH + nz1 * NYL + ny0
    nIndex(6) = nx0 * NYH * NZL + nz0 * NYH + ny1
    nIndex(7) = nx0 * NYL * NZL + nz0 * NYL + ny0

    for(i <- 0 until 8) {
      for(j <- 0 until 3) {
        edof(j*8+i) = nIndex(i)*24 + j*8 + i
      }
    }
    edof
  }
}

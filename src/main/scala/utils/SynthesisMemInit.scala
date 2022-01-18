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

    val fOffset = mapping(F.litValue.toInt) //F is the only vector to have values initialized at compile time
    val indexList = ListBuffer.empty[Int]

    //Original C-snippet is as follows
    /*
    for (int j = 0; j < ny; j++) {
      const int i = nx - 1;
      const int k = 0;
      const uint_fast32_t nidx = i * ny * nz + k * ny + j;
      F[3 * nidx + 2] = -1.0;
    }
     */
    //This means that the <ny> z-directed dofs in the topmost layer, with k=0, should all have value -1 (downwards force)
    //Due to our new EDOF mapping, we're finding the DOF's of all elements in top layer at k=0
    //Since these DOF's are at z=0, x=nelx-1 and the DOF's are z-directed, they must be in DOF banks 0 and 1, meaning indices 16 and 17 of the new DOFs, since grid sizes are ALWAYS even
    val i = NELX-1
    val k = 0
    for(j <- 0 until NELY) {
      val edof = getEdof(i,j,k)
      //We're only interested in the DOF's in z-direction for banks 0 and 1, entries 16 and 17
      indexList += edof(16)
      if(j != NELY-1) indexList += edof(17) //On the final iteration, the index for bank 1 is outside of the grid since grid size is always even
    }
    val indices = indexList.distinct //Some of the values were previously overlapping, remove those
    //Set these values in memory
    val mem = Array.ofDim[Double](8,wordsPerBank)
    val addresses = indices.map(_+fOffset)
    for(addr <- addresses) {
      val bank = addr % NUM_MEMORY_BANKS
      val offset = addr / NUM_MEMORY_BANKS
      mem(bank)(offset) = -1
    }

    //Create memory initialization files
    for(i <- 0 until NUM_MEMORY_BANKS) {
      val memFile = s"$memInitFileLocation/membank_$i.txt"
      val contents = mem(i).map(double2fixed).map(c => c & ((1L << FIXED_WIDTH)-1)) //Mask to preserve only FIXED_WIDTH lower bits
      writeMemInitFile(memFile, contents, 16) //Using 14 as length since FIXED_WIDTH=54 => 54/4=13.5 => 14 hex digits
    }
    wordsPerBank
  }

  /**
   * Compute the indices of the of the 24 degrees of freedom associated with the 8 corners of the element at (i,j,k) in the grid.
   *
   * @param i Current coordinate in the x-direction
   * @param j Current coordinate in the y-direction
   * @param k Current coordinate in the z-direction
   * @return An array of 24 integers holding the 24 DOF's of the specified element
   */
  def getEdof(i: Int, j: Int, k: Int): Array[Int] = {
    if(i == -1 && j == -1 && k == -1) { //Shorthand for elements outside of grid
      return Array.fill(24)(-1)
    }
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

package execution

import chisel3._
import chisel3.util._
import execution.KEWrapper.getKEslices
import utils.Config.{KE_SIZE, SIMULATION}
import utils.Fixed.{FIXED_WIDTH, double2fixed}

import scala.io.Source

/**
 * A wrapper around the KE-matrix, used to extract values from it. Implements [[KEWrapperIO]].
 * @param nelem The number of processing elements in the [[MatrixProcessingUnit]]. KE is split into submatrices of size
 *              (nelem x nelem)
 * @param sync Whether the output is synchronous or asynchronous. If sync=false, the output is immediatedly available after
 *             issuing a read. If sync=true, the output is available on the following clock cycle
 */
class KEWrapper(val nelem: Int, val sync: Boolean = false) extends Module {
  val io = IO(new KEWrapperIO(nelem))

  //Setup constants
  val KE = getKEslices(nelem)
  val subMatricesPerRow = KE_SIZE / nelem


  private val numSlices: Int = KE_SIZE * subMatricesPerRow
  //Create a lookup table mapping input x,y,col coordinates to the correct slice of memory
  //Each slice corresponding to one column of a submatrix
  val xymap: Vec[Vec[Vec[UInt]]] = Wire(Vec(subMatricesPerRow, Vec(subMatricesPerRow, Vec(nelem, UInt(log2Ceil(numSlices).W)))))
  for(x <- 0 until subMatricesPerRow) {
    for(y <- 0 until subMatricesPerRow) {
      for(col <- 0 until nelem) {
        xymap(y)(x)(col) := (y * KE_SIZE + x * nelem + col).U
      }
    }
  }
  val readLocation = xymap(io.keY)(io.keX)(io.keCol)

  //Creating width*subMatricesPerRow memory slices, each holding one slice of their respective submatrix
  //Depending on whether we're simulating or synthesizing, we'll want this memory to be instantiated as vec vs. Mem
  val keMem = Wire(Vec(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W))))
  for(i <- 0 until numSlices) {
    for(j <- 0 until nelem) {
      keMem(i)(j) := double2fixed(KE(i)(j)).S(FIXED_WIDTH.W)
    }
  }
  if(sync) {
    io.keVals := RegNext(keMem(readLocation))
  } else {
    io.keVals := keMem(readLocation)
  }
//  if(SIMULATION) {
//    val keMem = Wire(Vec(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W))))
//    for(i <- 0 until numSlices) {
//      for(j <- 0 until nelem) {
//        keMem(i)(j) := double2fixed(KE(i)(j)).S(FIXED_WIDTH.W)
//      }
//    }
//    if(sync) {
//      io.keVals := RegNext(keMem(readLocation))
//    } else {
//      io.keVals := keMem(readLocation)
//    }
//  } else {
//    //TODO SHould load actual KE values
//    val keMem = Wire(Vec(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W))))
//    for(i <- 0 until numSlices) {
//      for(j <- 0 until nelem) {
//        keMem(i)(j) := double2fixed(KE(i)(j)).S(FIXED_WIDTH.W)
//      }
//    }
//    if(sync) {
//      io.keVals := RegNext(keMem(readLocation))
//    } else {
//      io.keVals := keMem(readLocation)
//    }
//  }
}

/**
 * I/O ports for the KE wrapper module
 * @param nelem Number of processing elements / number of operands per operation
 */
class KEWrapperIO(val nelem: Int) extends Bundle {
  /** Input: X-coordinate of the submatrix to be processed */
  val keX = Input(UInt(log2Ceil(KE_SIZE/nelem).W))
  /** Input: Y-coordinate of the submatrix to be processed */
  val keY = Input(UInt(log2Ceil(KE_SIZE/nelem).W))
  /** Input: The column of the submatrix to be extracted */
  val keCol = Input(UInt(log2Ceil(nelem).W))
  /** Output: one column of a submatrix, as specified by the input variables */
  val keVals = Output(Vec(nelem, SInt(FIXED_WIDTH.W)))
}

object KEWrapper {
  /**
   * Gets the 2D-array representing the KE-matrix
   * @return A 2D-array of doubles representing the KE-matrix values
   */
  def getKEMatrix(): Array[Array[Double]] = {
    val KE = Array.ofDim[Double](KE_SIZE, KE_SIZE)
    if(!SIMULATION) {
      val src = Source.fromFile("src/resources/ke.csv")
      val lines = src.getLines().toArray
      if(lines.length != KE_SIZE) {
        throw new IllegalArgumentException("KE matrix CSV file must have exactly 24 lines")
      }
      for(i <- 0 until KE_SIZE) {
        val tokens = lines(i).split(",")
        if(tokens.length != KE_SIZE) {
          throw new IllegalArgumentException("Each line of the KE matrix in the CSV file must have exactly 24 items")
        }
        for(j <- 0 until KE_SIZE) {
          KE(i)(j) = tokens(j).toDouble
        }
      }
    } else {
      for (i <- 0 until KE_SIZE) {
        for (j <- 0 until KE_SIZE) {
          KE(i)(j) = i * KE_SIZE + j
        }
      }
    }
    KE
  }

  def main(args: Array[String]): Unit = {
    printKE()
  }

  def printKE(): Unit = {
    val KE = getKEMatrix()
    for (i <- 0 until KE_SIZE) {
      for (j <- 0 until KE_SIZE) {
        print(s"${KE(i)(j)}, ")
      }
      println()
    }
  }

  /**
   * Partitions the KE-matrix into a number of equally sized slices. Each slices returned has size (nelem x 1),
   * and a total of (nelem*nelem*2) slices are returned.
   * Each slice contains one column of one submatrix of the KE matrix. Slices [0;nelem-1] are from the submatrix at
   * (x=0,y=0), the next `nelem` slices are from the submatrix at (x=0,y=1) etc.
   *
   * @param nelem The number of processing elements.
   * @return (nelem*nelem*2) slices, holding all submatrix slices in the KE array
   */
  def getKEslices(nelem: Int): Array[Array[Double]] = {
    val width = KE_SIZE
    if(width % nelem != 0) {
      throw new IllegalArgumentException("Can only split KE matrix into equally sized chunks")
    }
    val KE = getKEMatrix()

    val numSlices = (math.pow(width/nelem,2)*nelem).toInt
    val keSlices = Array.ofDim[Double](numSlices, nelem)

    for(y <- 0 until width/nelem) {
      for(x <- 0 until width/nelem) {
        for(c <- 0 until nelem) {
          for(r <- 0 until nelem) {
            keSlices(y*width + x*nelem + c)(r) = KE(y*nelem+r)(x*nelem+c)
          }
        }
      }
    }
    keSlices
  }
}
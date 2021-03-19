package vector

import chisel3._
import chisel3.util._
import utils.Fixed.FIXED_WIDTH
import KEWrapper.{partitionKE, extractWithXYCol}
import utils.Fixed.double2fixed

/**
 * A wrapper around the KE-matrix, used to extract values from it. Implements [[KEWrapperIO]].
 * @param nelem The number of processing elements in the [[MatrixProcessingUnit]]. KE is split into submatrices of size
 *              (nelem x nelem)
 * @param sync Whether the output is synchronous or asynchronous. If sync=false, the output is immediatedly available after
 *             issuing a read. If sync=true, the output is available on the following clock cycle
 * @param simulation Whether the wrapper should be instantiated for simulation or synthesis. When simulating,
 *                   the KE matrix is created with a Vec of Vecs. With synthesizing, it is a memory bank
 *                   Should be true when simulating, false if not
 */
class KEWrapper(val nelem: Int, val sync: Boolean = false, val simulation: Boolean = true) extends Module {
  val io = IO(new KEWrapperIO(nelem))

  //Setup constants
  val KE = partitionKE(nelem)
  val numSubmatrices = KE.length
  val KEwidth = KEWrapper.width
  val subMatricesPerRow = KEwidth / nelem

  private val numSlices: Int = KEwidth * subMatricesPerRow
  //Create a lookup table mapping input x,y,col coordinates to the correct slice of memory
  //We're creating a large number of slices, each slice corresponding to one column of a submatrix
  val xymap = Wire(Vec(subMatricesPerRow, Vec(subMatricesPerRow, Vec(nelem, UInt(log2Ceil(numSlices).W)))))
  for(x<- 0 until subMatricesPerRow) {
    for(y <- 0 until subMatricesPerRow) {
      for(col <- 0 until nelem) {
        xymap(y)(x)(col) := (y * KEwidth + x * nelem + col).U
      }
    }
  }
  val readLocation = xymap(io.in.keY)(io.in.keX)(io.in.keCol)

  //Creating width*subMatricesPerRow memory slices, each holding one slice of their respective submatrix
  //Depending on whether we're simulating or synthesizing, we'll want this memory to be instantiated as vec vs. Mem
  if(simulation) {
    val keMem = Wire(Vec(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W))))
    for(keY <- 0 until subMatricesPerRow) {
      for (keX <- 0 until subMatricesPerRow) {
        for (col <- 0 until nelem) {
          val keVals = extractWithXYCol(KE, keX, keY, col, subMatricesPerRow, nelem)
          for(i <- 0 until nelem) {
            keMem(keY*KEwidth + keX*nelem + col)(i) := double2fixed(keVals(i)).S
          }
        }
      }
    }
    if(sync) {
      io.out.keVals := RegNext(keMem(readLocation))
    } else {
      io.out.keVals := keMem(readLocation)
    }
  } else {
    //Notice: When synthesizing, this defaults to a bunch of 0's. Need to work on how to pre-map the correct memory values in here
    //TODO: Right now this just synthesizes to absolutely nothing. Probably because we don't have a write port.
    val keMem = if(sync) {
      SyncReadMem(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W)))
    } else {
      Mem(numSlices, Vec(nelem, SInt(FIXED_WIDTH.W)))
    }
    keMem.write(20.U, VecInit(Seq(1.S, 1.S)))
    io.out.keVals := keMem.read(readLocation)
  }
}

/**
 * I/O ports for the KE wrapper module
 * @param nelem Number of processing elements / number of operands per operation
 */
class KEWrapperIO(val nelem: Int) extends Bundle {
  val in = Input(new KEWrapperInput(nelem))
  val out = Output(new KEWrapperOutput(nelem))

  /**
   * Input ports for the KE wrapper module
   * @param nelem Number of processing elements / number of operands per operation
   */
  class KEWrapperInput(val nelem: Int) extends Bundle {
    /** X-coordinate of the submatrix to be processed */
    val keX = UInt(log2Ceil(KEWrapper.width/nelem).W)
    /** Y-coordinate of the submatrix to be processed */
    val keY = UInt(log2Ceil(KEWrapper.width/nelem).W)
    /** The column of the submatrix to be extracted */
    val keCol = UInt(log2Ceil(nelem).W)
  }

  /**
   * Output ports for the KE wrapper module
   * @param nelem Number of processing elements / number of operands per operation
   */
  class KEWrapperOutput(val nelem: Int) extends Bundle {
    /** One column of a submatrix, as specified by the input variables */
    val keVals = Vec(nelem, SInt(FIXED_WIDTH.W))
  }
}

object KEWrapper {
  val width = 24
  val KE = Array.ofDim[Double](width,width)
  for(i <- 0 until width) {
    for (j <- 0 until width) {
      KE(i)(j) = i*width + j
    }
  }

  def main(args: Array[String]): Unit = {
//    partitionKE(4).foreach(_.foreach(_.foreach(println)))
    printKE()
  }

  def printKE(): Unit = {
    for (i <- 0 until width) {
      for (j <- 0 until width) {
        print(s"${KE(i)(j)}, ")
      }
      println()
    }
  }

  /**
   * Partitions the KE-matrix into a number of equally sized matrices. Each submatrix returned has size (nelem x nelem),
   * and a total of (24/nelem)^2^ submatrices are returned.
   *
   * @param nelem The number of elements in each submatrix. Must be a divisor of 24 such that 24 mod nelem == 0
   * @return A list of submatrices where the first (24/nelem) 2d-arrays belong to the first row,
   *         the next (24/nelem) arrays to the next row, etc
   */
  def partitionKE(nelem: Int): Array[Array[Array[Double]]] = {
    val width = KE.length
    if(width % nelem != 0) {
      throw new IllegalArgumentException("Can only split KE matrix into equally sized chunks")
    }

    val subMatricesPerRow = width/nelem
    val numSubMatrices = subMatricesPerRow*subMatricesPerRow
    val retVal = Array.ofDim[Double](numSubMatrices, nelem, nelem)

    for(i <- 0 until subMatricesPerRow) {
      for(j <- 0 until subMatricesPerRow) {
        for(k <- 0 until nelem) {
          for(l <- 0 until nelem) {
            retVal(i*subMatricesPerRow+j)(k)(l) = KE(i*nelem+k)(j*nelem+l)
          }
        }
      }
    }
    retVal
  }

  def extractWithXYCol(KE: Array[Array[Array[Double]]], x: Int, y: Int, col: Int, smpr: Int, nelem: Int): Array[Double] = {
    val submatrix = y*smpr + x
    val retVal = Array.ofDim[Double](nelem)
    for(i <- 0 until nelem) {
      retVal(i) = KE(submatrix)(i)(col)
    }
    retVal
  }
}
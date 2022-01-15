package execution

import chisel3._
import chisel3.util._
import utils.Config._
import utils.Fixed.FIXED_WIDTH
import chisel3.util.experimental.{loadMemoryFromFile, loadMemoryFromFileInline}

import scala.io.Source

class KEMatrix(val sync: Boolean = true) extends Module {
  val io = IO(new KEIO)

  /** Number of slices for each KE matrix */
  val numSlices = KE_SIZE * SUBVECTORS_PER_VREG //SPV = 3 right now
  /** Number of entries in each memory bank */
  val numEntries = numSlices * 4

  //When using concatenated access, numEntries becomes larger.
  //2 bits for iteration value, l2c(KE_SIZE/NP) for both X and Y coords, l2c(NP) for column access
//  val addrWidth = 2 + log2Ceil(KE_SIZE/NUM_PROCELEM)*2 + log2Ceil(NUM_PROCELEM)
//  val numEntries = Math.pow(2,addrWidth).toInt
//  val addr: UInt = Cat(io.keIter, io.keY, io.keX, io.keCol)

  val mem = for(i <- 0 until NUM_PROCELEM) yield {
    if(sync) SyncReadMem(numEntries, SInt(FIXED_WIDTH.W)) else Mem(numEntries, SInt(FIXED_WIDTH.W))
  }
  //Load sliced KE files as memory initialization values
  for(i <- 0 until NUM_PROCELEM) {
    if(INLINE) {
      loadMemoryFromFileInline(mem(i), s"src/resources/ke/ke_$i.txt")
    } else {
      loadMemoryFromFile(mem(i), s"src/resources/ke/ke_$i.txt")
    }
  }

  //We want a mapping from any combination of iteration, y, x and col coordinate
  //iterations: 4 possible values
  //y coordinate: SUBVECTORS_PER_VREG possibilities
  //x coordinate: SUBVECTORS_PER_VREG possibilites
  //col: At most NUM_PROCELEM entries
  val xymap: Vec[Vec[Vec[Vec[UInt]]]] = Wire(
    Vec(4,
      Vec(SUBVECTORS_PER_VREG,
        Vec(SUBVECTORS_PER_VREG,
          Vec(NUM_PROCELEM, UInt(log2Ceil(numEntries+1).W))))))
  //log2ceil(numEntries) since these are the UInts mapping into our memory banks

  for(i <- 0 until 4) {
    for(y <- 0 until SUBVECTORS_PER_VREG) {
      for(x <- 0 until SUBVECTORS_PER_VREG) {
        for(col <- 0 until NUM_PROCELEM) {
          xymap(i)(y)(x)(col) := (i*numSlices + y*KE_SIZE + x*NUM_PROCELEM + col).U
        }
      }
    }
  }

  val rdData: Vec[SInt] = Wire(Vec(NUM_PROCELEM, SInt(FIXED_WIDTH.W)))
  val addr: UInt = xymap(io.keIter)(io.keY)(io.keX)(io.keCol)

  //Index into all memory banks using this address
  for(i <- 0 until NUM_PROCELEM) {
    rdData(i) := mem(i).read(addr)
  }

  //Output that readdata
  io.keVals := rdData

}

object KEMatrix {
  /**
   * Gets the 2D-array representing the KE-matrix
   * @param iter Which iteration-transformed KE-matrix to get
   * @return A 2D-array of doubles representing the KE-matrix values
   */
  def getKEMatrix(iter: Int): Array[Array[Double]] = {
    require(0 <= iter && iter < 8)

    val KE = Array.ofDim[Double](KE_SIZE, KE_SIZE)
    val src = Source.fromFile(f"src/resources/ke-${iter}.csv")
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
    src.close()
    KE
  }


  /**
   * Partitions the KE-matrix into a number of equally sized slices. Each slices returned has size (nelem x 1),
   * and a total of (KE_SIZE^2^/NELEM) slices are returned.
   * Each slice contains one column of one submatrix of the KE matrix. Slices [0;nelem-1] are from the submatrix at
   * (x=0,y=0), the next `nelem` slices are from the submatrix at (x=1,y=0) etc. (x,y)=(0,0) is the top-left submatrix
   *
   * @param nelem The number of processing elements.
   * @return (KE_SIZE^2^/NELEM) slices, holding all submatrix slices in the KE array
   */
  def getKEslices(iter: Int): Array[Array[Double]] = {
    require(KE_SIZE % NUM_PROCELEM == 0, "Can only split KE matrix into equally sized chunks")

    val width = KE_SIZE
    val KE = getKEMatrix(iter)

    val numSlices = (math.pow(width/NUM_PROCELEM,2)*NUM_PROCELEM).toInt
    val keSlices = Array.ofDim[Double](numSlices, NUM_PROCELEM)

    for(y <- 0 until width/NUM_PROCELEM) {
      for(x <- 0 until width/NUM_PROCELEM) {
        for(c <- 0 until NUM_PROCELEM) {
          for(r <- 0 until NUM_PROCELEM) {
            keSlices(y*width + x*NUM_PROCELEM + c)(r) = KE(y*NUM_PROCELEM+r)(x*NUM_PROCELEM+c)
          }
        }
      }
    }
    keSlices
  }
}

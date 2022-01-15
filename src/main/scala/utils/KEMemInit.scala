package utils

import chisel3.util.log2Ceil
import execution.KEMatrix.getKEslices
import utils.Config._
import utils.Fixed.double2fixed

object KEMemInit extends App {

  def apply(memInitFileLocation: String = "src/resources/ke"): Unit = {
    //Get sliced versions of KE0-KE3
    val KE = for(i <- 0 until 4) yield {
      getKEslices(i)
    }

    //Flatten into an Array[Array[Double]], transpose to have 8 arrays of 288 elements
    val slices = KE.flatten.transpose

    //Write to meminit files
    for(i <- slices.indices) {
      val slice = slices(i).map(double2fixed).toArray
      Assembler.writeMemInitFile(f"$memInitFileLocation/ke_$i.txt", slice, 16)
    }

    //New version
    //Still get slices, flatten and transpose
    //Then, create 8 new Arrays of size 2^(2+2*log2ceil(KE_SIZE/NUM_PROCELEM)+log2ceil(NUM_PROCELEM)
    //These hold the new entries for each memory
    //Then, go throughall vectors
    //index % NUM_PROCELEM = col
    //index/NUM_PROCELEM % (KE_SIZE/NUM_PROCELEM) = X
    //index/(NUM_PROCELEM*KE_SIZE/NUM_PROCELEM) <=> index/KE_SIZE % (KE_SIZE/NUM_PROCELEM) = Y
    //index/(KE_SIZE^2/NUM_PROCELEM) = iter

    val width_col = log2Ceil(NUM_PROCELEM)
    val width_X = log2Ceil(KE_SIZE/NUM_PROCELEM)
    val width_Y = log2Ceil(KE_SIZE/NUM_PROCELEM)
    val width_iter = log2Ceil(4)
    val newSlices = Array.ofDim[Double](8, Math.pow(2,width_iter + width_Y + width_X + width_col).toInt)
    for(i <- slices(0).indices) {
      val col = i % NUM_PROCELEM
      val X = i/NUM_PROCELEM % (KE_SIZE/NUM_PROCELEM)
      val Y = i/KE_SIZE % (KE_SIZE/NUM_PROCELEM)
      val iter = i/(KE_SIZE*KE_SIZE/NUM_PROCELEM)

      val index = col | (X << width_col) | (Y << width_col+width_X) | (iter << width_col+width_X+width_Y)

      for(j <- slices.indices) {
        newSlices(j)(index) = slices(j)(i)
      }
    }
    for(i <- newSlices.indices) {
      val slice = newSlices(i).map(double2fixed).toArray
      Assembler.writeMemInitFile(f"$memInitFileLocation/ke_${i}_n.txt", slice)
    }
  }

  /*
  index 100:
  fra KE-matrix nr. 1 (større end 72) => iter
  100-72=28 => vi er på y=1, x=0, col=4

   */

  apply()
}

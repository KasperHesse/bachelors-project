package test

//import utils.Config._

object EdofGeneration extends App {

  val i = 0
  val j = 0
  val k = 0

  val NY = 5
  val NZ = 5

  val edof1 = edofGeneration(0,2,3)
  val edof2 = edofGen2(0,2,3)
  (edof1,edof2).zipped.foreach((a,b) => println(a == b))
//  edofGeneration(0,2,3).foreach(a => println(f"$a%4d (${a % 8})"))
  //24-26
  //3-5
  //30-32
  //9-11

  def edofGeneration(i: Int, j: Int, k: Int): Array[Int] = {

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

    //NYH = ceil(NY/2)
    //NYL = NY/2
    //NZH = ceil(NZ/2)
    //NZL = NZ/2
    //nx0 = i/2
    //nx1 = i/2+i(0)
    //ny0 = j/2
    //ny1 = j/2+j(0)
    //nz0 = k/2
    //nz1 = k/2+k(0)
    //4 combinations of NYH, NZH, NYL and NZL
    //2 input versions of nx => 2 LUTs
    // a total of NX possible inputs => NX*2*4 mappings?

    nIndex(0) = nx0 * NYH * NZH + nz1 * NYH + ny1
    nIndex(1) = nx0 * NYL * NZH + nz1 * NYL + ny0
    nIndex(2) = nx0 * NYH * NZL + nz0 * NYH + ny1
    nIndex(3) = nx0 * NYL * NZL + nz0 * NYL + ny0
    nIndex(4) = nx1 * NYH * NZH + nz1 * NYH + ny1
    nIndex(5) = nx1 * NYL * NZH + nz1 * NYL + ny0
    nIndex(6) = nx1 * NYH * NZL + nz0 * NYH + ny1
    nIndex(7) = nx1 * NYL * NZL + nz0 * NYL + ny0

    for(i <- 0 until 8) {
      for(j <- 0 until 3) {
        edof(j*8+i) = nIndex(i)*24 + j*8 + i
      }
    }
    edof
  }

  def edofGen2(i: Int, j: Int, k: Int): Array[Int] = {
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

    nIndex(0) = nx0 * NYH * NZH + nz1 * NYH + ny1
    nIndex(1) = nx0 * NYL * NZH + nz1 * NYL + ny0
    nIndex(2) = nx0 * NYH * NZL + nz0 * NYH + ny1
    nIndex(3) = nx0 * NYL * NZL + nz0 * NYL + ny0
    nIndex(4) = nx1 * NYH * NZH + nz1 * NYH + ny1
    nIndex(5) = nx1 * NYL * NZH + nz1 * NYL + ny0
    nIndex(6) = nx1 * NYH * NZL + nz0 * NYH + ny1
    nIndex(7) = nx1 * NYL * NZL + nz0 * NYL + ny0

    for(i <- 0 until 8) {
      for(j <- 0 until 3) {
        edof(j*8+i) = nIndex(i)*24 + j*8 + i
      }
    }
    edof
  }

}

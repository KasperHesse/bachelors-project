package vector

import chisel3._
import chiseltest._
import utils.Fixed._
import org.scalatest.{FlatSpec, Matchers}

class KEWrapperSpec  extends FlatSpec with ChiselScalatestTester with Matchers{
  behavior of "KE matrix wrapper"

  def testKEWrapper(dut: KEWrapper, nelem: Int): Unit = {
    def extractWithXYCol(KE: Array[Array[Array[Double]]], x: Int, y: Int, col: Int, smpr: Int, nelem: Int): Array[Double] = {
      val submatrix = y*smpr + x
      val retVal = Array.ofDim[Double](nelem)
      for(i <- 0 until nelem) {
        retVal(i) = KE(submatrix)(i)(col)
      }
      retVal
    }
    //Using a for-loop to go through every single value, reading out all possible configurations
    val KE = KEWrapper.partitionKE(nelem)
    val subMatricesPerRow = KEWrapper.width / nelem

    for(keY <- 0 until subMatricesPerRow) {
      for(keX <- 0 until subMatricesPerRow) {
        for(col <- 0 until nelem) {
          dut.io.in.keX.poke(keX.U)
          dut.io.in.keY.poke(keY.U)
          dut.io.in.keCol.poke(col.U)
          dut.clock.step()
          val res = extractWithXYCol(KE, keX, keY, col, subMatricesPerRow, nelem)

          print(s"keX=$keX, keY=$keY, col=$col")
          print(s" Vector values: [")
          for(i <- 0 until nelem) {
            print(s"${sint2double(dut.io.out.keVals(i).peek)}, ")
          }
          println("]")
          val outs = dut.io.out.keVals
          for(i <- 0 until nelem) {
            assert(sint2double(outs(i).peek) == res(i))
          }
        }
      }
    }
  }

  val nelem = 8
  val simulation = true
  it should "allow us to read out the values of the KE-matrix" in {
    test(new KEWrapper(nelem, simulation)) {c =>
      testKEWrapper(c, nelem)
    }
  }
}

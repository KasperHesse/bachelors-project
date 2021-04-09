//package vector
//
//import chisel3._
//import chiseltest._
//import utils.Fixed._
//import org.scalatest.{FlatSpec, Matchers}
//import utils.Config.{KE_SIZE, NUM_PROCELEM, SIMULATION}
//
//class KEWrapperSpec  extends FlatSpec with ChiselScalatestTester with Matchers{
//  behavior of "KE matrix wrapper"
//
//  def testKEWrapper(dut: KEWrapper, nelem: Int): Unit = {
//    def extractWithXYCol(KE: Array[Array[Array[Double]]], x: Int, y: Int, col: Int, smpr: Int, nelem: Int): Array[Double] = {
//      val submatrix = y*smpr + x
//      val retVal = Array.ofDim[Double](nelem)
//      for(i <- 0 until nelem) {
//        retVal(i) = KE(submatrix)(i)(col)
//      }
//      retVal
//    }
//    //Using a for-loop to go through every single value, reading out all possible configurations
//    val KE = KEWrapper.partitionKE(nelem)
//    val subMatricesPerRow = KE_SIZE / nelem
//
//    for(keY <- 0 until subMatricesPerRow) {
//      for(keX <- 0 until subMatricesPerRow) {
//        for(col <- 0 until nelem) {
//          dut.io.keX.poke(keX.U)
//          dut.io.keY.poke(keY.U)
//          dut.io.keCol.poke(col.U)
//          dut.clock.step()
//          val res = extractWithXYCol(KE, keX, keY, col, subMatricesPerRow, nelem)
//
//          print(s"keX=$keX, keY=$keY, col=$col")
//          print(s" Vector values: [")
//          for(i <- 0 until nelem) {
//            print(s"${fixed2double(dut.io.keVals(i).peek)}, ")
//          }
//          println("]")
//          val outs = dut.io.keVals
//          for(i <- 0 until nelem) {
//            assert(fixed2double(outs(i).peek) == res(i))
//          }
//        }
//      }
//    }
//  }
//
//  it should "allow us to read out the values of the KE-matrix" in {
//    NUM_PROCELEM = 2
//    KE_SIZE = 4
//    SIMULATION = true
//    test(new KEWrapper(NUM_PROCELEM, SIMULATION)) {c =>
//      testKEWrapper(c, NUM_PROCELEM)
//    }
//  }
//}
//TODO Fix this file


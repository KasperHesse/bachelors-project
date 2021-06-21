package test

import chisel3._
import chisel3.util._
import utils.Assembler
import utils.Fixed._
import utils.Config._

class Test extends Module {
  val io = IO(new Bundle {
    val i = Input(UInt(3.W))
    val j = Input(UInt(3.W))
    val k = Input(UInt(3.W))
    val res = Output(UInt(10.W))
  })
  private val width = 2
  private val dop = 3
  private val portsize = 4
  //  val arr: Array[Array[Array[UInt]]] =
//    Array(
//      Array(
//        Array(1.U(4.W), 2.U(4.W)), Array(3.U(4.W), 4.U(4.W))),
//      Array(
//        Array(5.U(4.W), 6.U(4.W)), Array(7.U(4.W), 8.U(4.W))
//      ))
  val arr = Array.ofDim[UInt](2,dop,portsize)
  for(i <- 0 until width) {
    for (j <- 0 until dop) {
      for (k <- 0 until portsize) {
        arr(i)(j)(k) = (i*dop*portsize + j*portsize + k).U(10.W)
      }
    }
  }

    val b = Array.ofDim[Vec[Vec[UInt]]](width)
    for(j<- 0 until width) {
      val a = Array.ofDim[Vec[UInt]](dop)
      for(k <- 0 until dop) {
        a(k) = VecInit(arr(j)(k)) //a(k): Vec[UInt]
      }
      b(j) = VecInit(a) //b(j): Vec[Vec[UInt]]
    }
    val reg = RegInit(VecInit(b))

  io.res := reg(io.i)(io.j)(io.k)
}

object Test extends App {
  NX = 5
  NY = 5
  NZ = 5
  val vals = for(i <- 0 until 1024) yield {
    (scala.util.Random.nextDouble()*math.pow(2,32)).toLong
  }
  Assembler.writeMemInitFile("im.hex.txt", vals.toArray)


  utils.SynthesisMemInit.getEdof(0,0,0).foreach(a => println(s"$a, ${a % 8}"))

  val imm = imm2fixed(0.08578) //Smallest value that can be expressed in an immediate
  val imm2 =
//  val r = fixedMul(fixedMul(0.0078125, 0.015625), 0.015625)
    println(s"$imm\t ${fixed2double(imm)} ${imm.toBinaryString}")
//  println(r)
//  print(s"Normal: ${0x8000000000000000L}. Shifted: ${(1L << (64-1))}")
}
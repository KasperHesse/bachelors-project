package test

import chisel3._
import chisel3.util._
import utils.Assembler
import utils.Fixed._
import utils.Config._

import java.io.File

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

  def P(x: Long, s: String = ""): Unit = {
    println(f"$s ${fixed2double(x)}%.10f")
  }

  def D(x: Long, d: Double = 0.235043): Unit = {
    println(math.abs(fixed2double(x)-d))
  }

  def betterVolfrac(): Unit = {
    val start = imm2fixed(0.2)

    val e1 = imm2fixed(0.0546875)
    val e2 = fixedMul(e1, e1)
    val imm2 = fixedSub(start, e2)

    val f1 = imm2fixed(0.109375)
    val f2 = fixedMul(f1, f1)
    val f3 = fixedMul(f2, f2)
    val imm3 = fixedSub(imm2, f3)

    val g1 = imm2fixed(0.05446539631)
    val g2 = fixedMul(g1, g1)
    val g3 = fixedMul(g2, g2)
    val imm4 = fixedAdd(imm3, g3)

    P(start, "start")
    D(start, 0.2)
    P(imm2, "imm2")
    D(imm2, 0.2)
    P(imm3, "imm3")
    D(imm3, 0.2)

    P(g1, "g1")
    P(imm4, "imm4")
    D(imm4, 0.2)

    //Final result: Delta of 1.1e-7
  }

  def betterKeDiag(): Unit = {
    val start = imm2fixed(0.235043)

    val d1 = imm2fixed(0.0258457)
    val d2 = fixedMul(d1, d1)

    val imm2 = fixedAdd(start, d2)

    val e1 = imm2fixed(0.0491455)
    val e2 = fixedMul(e1, e1)
    val e3 = fixedMul(e1, e2)
    val imm3 = fixedAdd(imm2, e3)

    val f1 = imm2fixed(0.06293374700)
    val f2 = fixedMul(f1, f1)
    val f3 = fixedMul(f2, f2)
    val imm4 = fixedAdd(imm3, f3)

    P(start, "start")
    D(start)
    P(imm2, "imm2")
    D(imm2)
    P(imm3, "imm3")
    D(imm3)
    P(f1, "f1")
    P(imm4, "imm4")
    D(imm4)

    //Result: Delta of 5e-5
  }

  def betterOmega(): Unit = {
    val start = imm2fixed(0.6)



    val d1 = imm2fixed(0.03952847075)
    val d2 = fixedMul(d1, d1)
    val imm2 = fixedSub(start, d2)

    val e1 = imm2fixed(0.07779162548)
    val e2 = fixedMul(e1,e1)
    val e3 = fixedMul(e2, e2)
    val imm3 = fixedSub(imm2, e3)

    P(start, "start")
    D(start, 0.6)
    P(imm2, "imm2")
    D(imm2, 0.6)
    P(imm3, "imm3")
    D(imm3, 0.6)
  }

//  betterOmega()

  val a = string2fixed("3fffc000000000") //-1
  val b = string2fixed("3fffedf1d00c38") //-0.28211592489969917... very precise
  val c = string2fixed("3fffedf1d00c30") //-0.282115924928803..., almost as precise, no 4 LSB set
  val d = string2fixed("3fffee00000000") //-0.28125
  val e = string2fixed("00001200000000") //+0.28125

  def pf(v: SInt): Unit = println(fixed2double(v))

  val x = fixedMul(a,b)
  val y = fixedMul(a,c)
  val z = fixedMul(a,d)
  pf(a)
  pf(b)
  pf(c)
  pf(d)
  pf(e)

  println("\n")
  pf(x)
  pf(y)
  pf(z)

}
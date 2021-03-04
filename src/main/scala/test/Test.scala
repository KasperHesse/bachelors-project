package test

import chisel3._
import chisel3.util._

object Test extends App {
  print(s"Normal: ${0x8000000000000000L}. Shifted: ${(1L << (64-1))}")
}
//Peeking values that are
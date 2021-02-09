package utils

import chisel3._

class Interfaces {

}

class Float extends Bundle {
  val sign = Bool()
  val exp = UInt(11.W)
  val man = UInt(52.W)
}

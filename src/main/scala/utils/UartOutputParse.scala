package utils

import utils.Config.{ELEMS_PER_VSLOT, NELEMLENGTH, NELX, NELY, NELZ, NUM_MEMORY_BANKS}
import utils.Fixed._

import java.io.{BufferedWriter, FileWriter}
import scala.io._

/**
 * Helper object for parsing UART data read with Waveforms
 */
object UartOutputParse extends App {

  def iterationFromIJK(ijk: Array[Int]): Int = {
    require(ijk.length == 3, "ijk must have exactly 3 elements to calculate the iteration value")
    ijk.map(e => (e+2) % 2) match { //Must add 2 to ensure that elements with value -1 from neighbour lookup still match correctly
      case Array(0,0,0) => 0
      case Array(0,1,0) => 1
      case Array(0,0,1) => 2
      case Array(0,1,1) => 3
      case Array(1,0,0) => 4
      case Array(1,1,0) => 5
      case Array(1,0,1) => 6
      case Array(1,1,1) => 7
    }
  }

  def elementIndex(ijk: Array[Int]): Int = {
    val i = ijk(0)
    val j = ijk(1)
    val k = ijk(2)
    // (i/2*NELYH*NELZH + k/2*NELYH + j/2) << 3 | iteration
    val e = (i/2*((NELY+1)/2)*((NELZ+1)/2) + k/2*((NELY+1)/2) + j/2)*NUM_MEMORY_BANKS + iterationFromIJK(Array(i,j,k))
    if(i < 0 || i >= NELX || j < 0 || j >= NELY || k < 0 || k >= NELZ) {(e % 8) - 8} else e
  }

  def elementIndexCStyle(i: Int, j: Int, k: Int): Int = {
    i * NELY * NELZ + k * NELY + j
  }


  val name = "18jan_16h55m_6x6x6"
  val file = Source.fromFile(s"uart_dumps/$name.txt")

  //Get all text, concatenate it into one long line
  val text = file.getLines().toSeq.foldLeft("")((a,b) => a.concat(b))
  file.close()

  //Group each section of 7 bytes, reverse the bytes and concatenate into a hex string
  val textValues = text.split(" ").grouped(7).map(x => x.reverse.foldLeft("")((a, b) => a.concat(b))).toSeq
  println(s"Found ${textValues.size} values")

  //Convert hex strings to double values
  val values = textValues.map(x => fixed2double(string2fixed(x)))
  //On each iteration, we output 6 values:
  //x1: relres
  //x2: cgIter
  //x3: compliance
  //vol
  //change
  //loop
  //We know that at the end we will get NELEMLENGTH xphys values
  //We slice out the first values, group in sections of 6, write to another vcd file
  val (stats, xphys) = values.splitAt(values.length-NELEMLENGTH)

  val statFile = new BufferedWriter(new FileWriter(s"uart_dumps/${name}_stats.csv"))
  statFile.write("loop, relres, cgIter, compliance, vol, change, change2\n")
  stats.grouped(6).foreach(x => statFile.write(s"${x(5)},${x(0)},${x(1)},${x(2)},${x(3)},${x(4)},${x(4)/(ELEMS_PER_VSLOT*2)}\n"))

  statFile.close()

  val xphysFile = new BufferedWriter(new FileWriter(s"uart_dumps/${name}_xphys.csv"))
  xphysFile.write("elementIndex_scala,elementIndex_c,value\n")
  for(i <- 0 until NELX) {
    for(k <- 0 until NELZ) {
      for(j <- 0 until NELY) {
        val ei_scala = elementIndex(Array(i,j,k))
        val ei_c = elementIndexCStyle(i,j,k)
        xphysFile.write(f"${ei_scala},${ei_c},${xphys(ei_scala)}\n")
      }
    }
  }
  xphysFile.close()

}

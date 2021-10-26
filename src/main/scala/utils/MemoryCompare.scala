package utils

import scala.io.Source

/**
 * Compares the contents of two memory dump files
 */
object MemoryCompare extends App {

  val testName = "simple"
  val hash = "68758d"

  def apply(): Unit = {
    compare("F")
    compare("DV")
    compare("R")
  }

  def compare(name: String, delta: Double = 0.001): Unit = {

    val file1 = Source.fromFile(f"memdump/$testName/$hash/mem_$name.csv")
    val file2 = Source.fromFile(f"memdump_C/memdump_$name.csv")

    val lines1 = file1.getLines().toArray
    val lines2 = file2.getLines().toArray

    require(lines1.length == lines2.length, s"Files for [$name] must be of the same length. f1: ${lines1.length}, f2: ${lines2.length}")

    println(s"Comparing values for [$name]")
    var errCnt = 0
    for(i <- 1 until lines1.length) {
      val scala = lines1(i).split(',').last.toDouble
      val c = lines2(i).split(',').last.toDouble

      if(math.abs(scala-c) > delta) {
        println(f"ERROR at $i. Scala: $scala%.5f, C: $c%.5f, Delta: ${math.abs(scala-c)}%.5f")
        errCnt += 1
      }
    }
    if(errCnt > 0) {
      println(f"$errCnt errors found for [$name] at delta=$delta")
    }
    println("Comparison finished\n\n")

    file1.close()
    file2.close()
  }

  apply()

}

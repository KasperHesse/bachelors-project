package utils

import scala.io.Source

/**
 * Compares the contents of two memory dump files
 */
object MemoryCompare extends App {

  val testName = "gmd_cgiter"
  val hash = "466092"

  def apply(): Unit = {
    compare("DV")
    compare("R")
//    compare("INVD") //Not comparing INVD right now since 1/(small difference) => large difference
    compare("Z")
    compare("TMP")
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

  def checkTmpZvalues(): Unit = {
    val Z = Source.fromFile(f"memdump/$testName/$hash/mem_Z.csv")
    val TMP = Source.fromFile(f"memdump/$testName/$hash/mem_TMP.csv")
    val INVD = Source.fromFile(f"memdump/$testName/$hash/mem_invD.csv")
    val Zc = Source.fromFile(f"memdump_C/memdump_Z.csv")
    val TMPc = Source.fromFile(f"memdump_C/memdump_TMP.csv")

    val scalaZlines = Z.getLines().toArray
    val scalaTMPlines = TMP.getLines().toArray
    val cZlines = Zc.getLines().toArray
    val cTMPlines = TMPc.getLines().toArray
    val invdLines = INVD.getLines().toArray

    for (i <- 1 until scalaZlines.length) {
      val scalaZ = scalaZlines(i).split(",").last.toDouble
      val scalaTMP = scalaTMPlines(i).split(",").last.toDouble
      val cZ = cZlines(i).split(",").last.toDouble
      val cTMP = cTMPlines(i).split(",").last.toDouble
      val invd = invdLines(i).split(",").last.toDouble

      val delta = math.abs(scalaTMP-cTMP)*invd*0.6
      println(f"Z value in: C [$cZ], scala [$scalaZ], delta[$delta] scala+delta[${scalaZ+delta}], scala-delta[${scalaZ-delta}]")
    }
  }

  checkTmpZvalues()
//  apply()

}

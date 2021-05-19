package memory.membankinit

import pipeline.writeMemInitFile

object InitMemBanks extends App {

  /**
   * Initializes memory banks to some standard values that are easy to operate on
   */
  def apply(): Unit = {
    for(i <- 0 until 8) {
      val values = (for(j <- 0 until 6) yield {
        j*8+i
      }).toArray
      writeMemInitFile("src/test/scala/memory/membankinit/membank_" + i + ".txt", values)
    }
  }
}

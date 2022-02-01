package common

import chisel3._
import execution.{Decode, Opcode, OtypeLen, RtypeMod}
import execution.OtypeLen.{DOUBLE, NDOF, NELEMSTEP, NELEMVEC, SINGLE}
import memory.{ReadQueueBundle, genIJKmultiple, nextIJK}
import utils.Assembler
import utils.Config.NUM_MEMORY_BANKS
import utils.Config._
import utils.Fixed._

import scala.collection.mutable
import scala.io.Source

/**
 * A container class with handles to memory, register files and common control values when executing a simulation
 */
class SimulationContext {
  /** S-register file */
  var sReg: Array[SInt] = _
  /** X-register files. (0) is thread 0, (1) is thread 1 */
  var xReg: Array[Array[Array[SInt]]] = _
  /** V-register files. (0) is thread 0, (1) is thread 1 */
  var vReg: Array[Array[Array[SInt]]] = _
  /** Memory */
  var mem: Array[Array[SInt]] = _
  /** Instruction buffer holding current packet */
  val iBuffer = new InstructionBuffer
  /** Progress counter, keeping track of how many elements have been modified in each memory location */
  var progress: Int = 0
  /** Maximum progress value before an instruction packet is finished */
  var maxProgress: Int = 0
  /** Number of elements processed on each packet iteration */
  var progressIncr: Int = 0
  /** Number of elements processed in a MAC instruction */
  var MAClength: Int = 0
  /** Base index for each thread used when executing ld.vec or st.vec operations */
  var vecBaseIndex: Array[Int] = Array.ofDim[Int](2)
  /** Base IJK-tuples used when performing memory load/store operations. */
  val ijkBase: Array[Array[Int]] = Array.ofDim[Int](2,4)
  /** ID of current thread with access to execute stage */
  var execThread: Int = 1
  /** ID of current thread with access to memory stage */
  var memThread: Int = 0
  /** Container used to track results of execution */
  var results: Array[SInt] = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))
  /** Container used to track intermediate MAC results when processing MAC or RED instructions */
  var MACresults: Array[SInt] = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
  /** Flag indicating if a MAC instruction is the first MAC in an iteration. See [[common.handleMACSVandMACVV]] for details */
  var firstMAC: Boolean = true
  /** Flag indicating if a MAC instruction uses immediate values or register values. See [[common.handleMACSVandMACVV]] for details */
  var macUseImm: Boolean = false
  /** Immediate value used when performing MAC instructions. Will always be the same value for an entire MAC instruction */
  var macImmValue: SInt = 0.S
  /** Number of mac instructions seen in the current iteration */
  var macCnt: Int = 0
  /** Iteration values associated with each vector register in each thread */
  var vregIter: Array[Array[Int]] = Array.ofDim(2, VREG_SLOT_WIDTH)

  /** Signals asserted whenever a simulation thread is finished in its current packet section */
  private val signals: Array[Boolean] = Array.ofDim[Boolean](2)
  /** Signals pulled high after both threads have reached a thread swap breakpoint. Used for control flow */
  private val signalRelease: Array[Boolean] = Array.ofDim[Boolean](2)
  private var threadsFinished: Int = 0

  /**
   * Initializes this memory container, setting all registers to match their corresponding versions in the decode stage,
   * and initializing memory according to the given memory initialization files
   */
  def initialize(decode: Decode, memInitFileLocation: String): Unit = {
    this.sReg = decode.sRegFile.arr
    this.xReg = Array(decode.threads(0).xRegFile.arr, decode.threads(1).xRegFile.arr)
    this.vReg = Array(decode.threads(0).vRegFile.arr, decode.threads(1).vRegFile.arr)
    this.mem = Array.fill(NUM_MEMORY_BANKS, WORDS_PER_BANK)(0.S(FIXED_WIDTH.W))
    if(memInitFileLocation.nonEmpty) {
      for(bank <- 0 until NUM_MEMORY_BANKS) {
        val file = if(memInitFileLocation.takeRight(1).equals("/")) {
          s"${memInitFileLocation}membank_$bank.txt"
        } else {
          s"${memInitFileLocation}/membank_$bank.txt"
        }
        val src = Source.fromFile(file)
        for(line <- src.getLines().zipWithIndex) {
          mem(bank)(line._2) = utils.Fixed.string2fixed(line._1)
        }
      }
    }
  }

  /**
   * Sets up a number of different internal values.
   * Must be called before executing an instruction packet
   */
  def packetSetup(): Unit = {
    import OtypeLen._
    if(iBuffer.pstart.len.litValue == SINGLE.litValue) {
      maxProgress = 1
      progressIncr = 1
    } else if (iBuffer.pstart.len.litValue == DOUBLE.litValue) {
      maxProgress = 2
      progressIncr = 1
      MAClength = NDOFLENGTH
    } else if (iBuffer.pstart.len.litValue == NELEMVEC.litValue) {
      maxProgress = NELEMLENGTH
      progressIncr = ELEMS_PER_VSLOT
      MAClength = NELEMLENGTH
    } else if (iBuffer.pstart.len.litValue == NELEMSTEP.litValue) {
      maxProgress = NELEM
      progressIncr = 1
      MAClength = 1
    } else if (iBuffer.pstart.len.litValue == NDOF.litValue) {
      maxProgress = NDOFLENGTH
      progressIncr = ELEMS_PER_VSLOT
      MAClength = NDOFLENGTH
    } else if (iBuffer.pstart.len.litValue == NELEMDOF.litValue) {
      maxProgress = leastMultiple(XREG_DEPTH, NELEM)
      progressIncr = XREG_DEPTH
      MAClength = NELEMLENGTH
    }

    //We require a special check to modify simulation behaviour if only instruction is mac.vv/mac.sv
    if(iBuffer.exec.length == 1 && iBuffer.exec(0).op.litValue == Opcode.MAC.litValue &&
    iBuffer.exec(0).mod.litValue != RtypeMod.KV.litValue) {
      maxProgress = 1
      progressIncr = 1
    }

    this.progress = 0
    this.memThread = 0
    this.execThread = 1
    this.signals(0) = false
    this.signals(1) = false
    this.signalRelease(0) = false
    this.signalRelease(1) = false
    this.vecBaseIndex(0) = 0
    this.vecBaseIndex(1) = 0
    this.ijkBase(0) = Array(0,0,0,0)
    this.ijkBase(1) = Array(0,0,0,0)
    this.threadsFinished = 0
    this.results = Array.fill(VREG_DEPTH)(0.S(FIXED_WIDTH.W))
    this.MACresults = Array.fill(NUM_PROCELEM)(0.S(FIXED_WIDTH.W))
    this.macCnt = 0
    this.firstMAC = true
  }

  /**
   * Signal to the container that a thread is either finished processing store/load instructions,
   * or finished processing the executable R-type instructions
   */
  def signalSectionFinished(id: Int): Unit = {
    this.signals(id) = true
    if(this.signals.reduce((a,b) => a & b)) { //And-reduce
      val temp = this.memThread
      this.memThread = this.execThread
      this.execThread = temp

      this.signals(0) = false
      this.signals(1) = false
      this.signalRelease(0) = true
      this.signalRelease(1) = true

      this.progress += progressIncr
      println(f"PROGRESS: ${this.progress}/${this.maxProgress}")
    }
  }

  def signalThreadFinished(): Unit = {
    this.threadsFinished += 1
  }

  def getThreadsFinished: Int = {
    this.threadsFinished
  }

  def allThreadsFinished: Boolean = {
    this.threadsFinished == 2
  }

  /**
   * Called by a thread to await permission to swap. A thread must call [[signalSectionFinished]] immediatedly
   * before calling this method, and may not proceed before this returns true
   * @return
   */
  def awaitSwap(id: Int): Boolean = {
    if(this.signalRelease(id)) {
      this.signalRelease(id) = false
      true
    } else {
      false
    }
  }

  /**
   * Increases the base index used when the current memory thread performs ld.vec/st.vec operations
   * as well as the base IJK-tuple used when performing elementwise accesses
   */
  def increaseMemoryBase(): Unit = {
    this.vecBaseIndex(this.memThread) = this.vecBaseIndex(this.execThread) + ELEMS_PER_VSLOT //Vec progress always increments by this amount

    if(this.iBuffer.pstart.len.litValue == OtypeLen.NELEMSTEP.litValue) { //On nelemstep, we just move to the very next ijk value
      this.ijkBase(this.memThread) = nextIJK(this.ijkBase(this.execThread))
    } else { //On all others, we move forward by XREG_DEPTH ijk values compared to execThread
      this.ijkBase(this.memThread) = genIJKmultiple(start=Some(this.ijkBase(this.execThread)), XREG_DEPTH+1).last
    }
  }

  /**
   * Returns true when the simulation of the current packet is finished, and threads should go to the idle state
   * @return
   */
  def isFinished: Boolean = {
    this.progress >= this.maxProgress
  }
}

object MemoryContainerTest extends App {
  val mc = new SimulationContext
//  val decode = Module(new Decode)

  //Create memory init files with some simple values. Then read these back in
  //Create very simple 2x4 memory array
  utils.Config.NUM_MEMORY_BANKS = 2
  mc.mem = Array.fill(NUM_MEMORY_BANKS, 4)(0.S(FIXED_WIDTH.W))

  val memInitFileLocation = "meminit"
  //Setup the values
  val v1 = Array[Long](-2, -1, 0, 1).map(utils.Fixed.long2fixed)
  val v2 = Array[Long](-4, -3, 3, 4).map(utils.Fixed.long2fixed)
  Assembler.writeMemInitFile("meminit/membank_0.txt", v1.map(utils.Fixed.fixed2long), 16)
  Assembler.writeMemInitFile("meminit/membank_1.txt", v2.map(utils.Fixed.fixed2long), 16)

  if(memInitFileLocation.nonEmpty) {
    for(bank <- 0 until NUM_MEMORY_BANKS) {
      val file = if(memInitFileLocation.takeRight(1).equals("/")) {
        s"${memInitFileLocation}membank_$bank.txt"
      } else {
        s"${memInitFileLocation}/membank_$bank.txt"
      }
      val src = Source.fromFile(file)
      for(line <- src.getLines().zipWithIndex) {
        val x = utils.Fixed.string2fixed(line._1)
        val y = fixed2long(x)
        println(f"$x%-30s\t\t$y%2d")
      }
    }
  }
}
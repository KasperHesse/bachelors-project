package pipeline

import chisel3._
import chisel3.util._
import utils.Fixed._

/**
 * The vector register file. Implements [[VectorRegFileIO]].
 * If 'portsize' is equal to 'depth', a full vector of 'depth' elements can be read and written during each clock cycle.
 * rdMask1, rdMask2 and wrMask must be tied to 1 for this to work
 * If portsize < depth, portsize-wide subvectors can be read out on each clock cycle. In this case, rdMask(1,2) and wrMask
 * specify which subvector to read out. (If eg. depth=32 and portsize=8, valid values for the mask ports are 0,1,2,3).
 * @param width The number of vector registers.
 * @param depth The number of values stored in each vector register. Total number of values stored is width*depth
 * @param portsize The number of elements that can be read/written in one clock cycle.
 * @note When simulating, the register file is pre-populated with values. Vector register 0 has values [0;depth-1],
 *       register 2 has values [depth;2*depth-1] etc
 *
 */
class VectorRegisterFile(val width: Int, val depth: Int, val portsize: Int) extends Module {
  require(depth % portsize == 0, "Depth must be an integer multiple of portsize")
  require(portsize <= depth, "port size must be less than or equal to depth")

  val io = IO(new VectorRegFileIO(width, depth, portsize))

  //This field is only used for simulation purposes
  var arr: Array[Array[Array[SInt]]] = Array.ofDim[SInt](0,0,0)
  if(utils.Config.SIMULATION) {
    arr = ArrayInitaliser(width, depth, portsize)
    //This is some really ugly code, but it works.
    //It builds up the register file using VecInit on the subvectors,
    // stores them in array, creates nested Vecs, and finally initialises the register file
    val b = Array.ofDim[Vec[Vec[SInt]]](width)
    for(j<- 0 until width) {
      val a = Array.ofDim[Vec[SInt]](depth/portsize)
      for(k <- 0 until depth/portsize) {
        a(k) = VecInit(arr(j)(k)) //a(k): Vec[SInt].
      }
      b(j) = VecInit(a) //b(j): Vec[Vec[UInt]]
    }
    val regFile = RegInit(VecInit(b))

    io.rdData1 := regFile(io.rs1)(io.rdMask1)
    io.rdData2 := regFile(io.rs2)(io.rdMask2)

    when(io.we) {
      regFile(io.rd)(io.wrMask) := io.wrData
    }
  } else {
    //Currently implemented with memory, could just as well be implemented with registers
    val regFile = SyncReadMem(width, Vec(depth / portsize, Vec(portsize, SInt(FIXED_WIDTH.W))))

    io.rdData1 := regFile.read(io.rs1)(io.rdMask1)
    io.rdData2 := regFile.read(io.rs2)(io.rdMask2)

    //Create a vector of 0's, one subvector of which is taken up by our input vector
    //This is done to leverage the masked writes that SyncReadMem provides
    val writeVector = Wire(Vec(depth / portsize, Vec(portsize, SInt(FIXED_WIDTH.W))))
    for (i <- 0 until depth / portsize) {
      writeVector(i) := VecInit(Seq.fill(portsize)(0.S(FIXED_WIDTH.W)))
    }
    writeVector(io.wrMask) := io.wrData
    when(io.we) {
      regFile.write(io.rd, writeVector, UIntToOH(io.wrMask, depth / portsize).asBools())
    }
  }
}

class VectorRegFileIO(width: Int, depth: Int, portsize: Int) extends Bundle {
  /** Write enable bit */
  val we = Input(Bool())
  /** Write destination register */
  val rd = Input(UInt(log2Ceil(width).W)) //Destination register 1
  /** Source register 1 */
  val rs1 = Input(UInt(log2Ceil(width).W)) //Register select 1
  /** Source register 2 */
  val rs2 = Input(UInt(log2Ceil(width).W)) //Register select 2
  /** Mask for read port 1 */
  val rdMask1 = Input(UInt(log2Ceil(depth/portsize+1).W))
  /** Mask for read port 2 */
  val rdMask2 = Input(UInt(log2Ceil(depth/portsize+1).W))
  /** Write mask */
  val wrMask = Input(UInt(log2Ceil(depth/portsize+1).W))
  /** Write data */
  val wrData = Input(Vec(portsize, SInt(FIXED_WIDTH.W)))
  /** Read data port 1 */
  val rdData1 = Output(Vec(portsize, SInt(FIXED_WIDTH.W))) //Read data 1
  /** Read data port 2 */
  val rdData2 = Output(Vec(portsize, SInt(FIXED_WIDTH.W))) //Read data 2

  override def cloneType = (new VectorRegFileIO(width, depth, portsize)).asInstanceOf[this.type]
}

object ArrayInitaliser {
  def apply(width: Int, depth: Int, portsize: Int): Array[Array[Array[SInt]]] = {
    val arr = Array.ofDim[SInt](width, depth/portsize, portsize)

    for(w <- 0 until width) {
      for(d <- 0 until depth/portsize) {
        for(p <- 0 until portsize) {
          arr(w)(d)(p) = long2fixed(w*depth+d*portsize+p)
        }
      }
    }
    arr
  }
}
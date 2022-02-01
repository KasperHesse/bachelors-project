package memory

import chisel3._
import chisel3.util._
import utils.Config.{NUM_VREG, NUM_SREG, VREG_DEPTH}
import utils.Fixed.FIXED_WIDTH
import utils.Config._

class VectorRegisterFileIO(width: Int, depth: Int, masksize: Int) extends Bundle {
  val in = Input(new VectorRegisterFileInput(width, depth, masksize))
  val out = Output(new VectorRegisterFileOutput)
//  override def cloneType = (new VectorRegisterFileIO(width, depth, masksize)).asInstanceOf[this.type]

  class VectorRegisterFileInput(width: Int, depth: Int, masksize: Int) extends Bundle {
    /** Write enable bit */
    val we = Bool()
    /** Destination register */
    val rd = UInt(log2Ceil(width).W)
    /** Write mask. Used to select which portion of the register is written into.
     * If eg depth=32 and masksize=8, wrMask will support the values 0,1,2,3.
     * If mask=1, elements [8:15] in field rd will be overwritten with the data in wrData. */
    val wrMask = UInt(log2Ceil(depth/masksize+1).W)
    /** Read mask. Used to select subvector read. See [[wrMask]] for more explanation */
    val rdMask = UInt(log2Ceil(depth/masksize+1).W)
    /** Register select 1  */
    val rs1 = UInt(log2Ceil(width).W)
    /** Register select 2 */
    val rs2 = UInt(log2Ceil(width).W)
    /** Write data, goes into the register specified by rd. Is available for read on the following clock cycle */
    val wrData = Vec(masksize, SInt(FIXED_WIDTH.W))

//    override def cloneType = (new VectorRegisterFileInput(width, depth, masksize)).asInstanceOf[this.type]
  }

  class VectorRegisterFileOutput extends Bundle {
    /** Read data 1. Read from the port specified by in.rs1 */
    val rdData1 = Vec(masksize, SInt(FIXED_WIDTH.W))
    /** Read data 2. Read from the port specified by in.rs2 */
    val rdData2 = Vec(masksize, SInt(FIXED_WIDTH.W))
  }
}


/**
 * An abstract class defining the IO available for a register file
 * @param width The number of entries in the register file
 * @param datatype The datatype stored in the register file
 */
abstract class RegisterFile(val width: Int, val datatype: Data) extends Module {
  val io = IO(new RegisterFileIO(width, datatype))

  val regFile = Reg(Vec(width, datatype))

  io.out.rdData1 := regFile(io.in.rs1)
  io.out.rdData2 := regFile(io.in.rs2)

  when(io.in.we && io.in.rd =/= 0.U) {
    regFile(io.in.rd) := io.in.wrData
  }
}

class RegisterFileIO(width: Int, val datatype: Data) extends Bundle {
  val in = Input(new RegisterFileInput(width))
  val out = Output(new RegisterFileOutput)

  class RegisterFileInput(width: Int) extends Bundle {
    /** Write enable bit */
    val we = Bool()
    /** Destination register */
    val rd = UInt(log2Ceil(width).W)
    /** Register select 1  */
    val rs1 = UInt(log2Ceil(width).W)
    /** Register select 2 */
    val rs2 = UInt(log2Ceil(width).W)
    /** Write data, goes into the register specified by rd. Is available for read on the following clock cycle */
    val wrData = datatype
  }

  class RegisterFileOutput extends Bundle {
    /** Read data 1. Read from the port specified by in.rs1 */
    val rdData1 = datatype
    /** Read data 2. Read from the port specified by in.rs2 */
    val rdData2 = datatype
  }
}
//import chisel3._
//import chisel3.stage.ChiselStage
//
//class Float extends Bundle {
//  val sign = Bool()
//  val exponent = UInt(11.W)
//  val mantissa = UInt(52.W)
//}
//
//class Fixed extends Bundle {
//  val sign = Bool()
//  val value = UInt(63.W)
//}
//
//class FixedOrFloat(f : Boolean) extends Bundle {
//  val fixed = if (f) new Fixed else None
//  val float = if (!f) new Float else None
//  override def cloneType = new FixedOrFloat(f).asInstanceOf[this.type]
//}
//
//class Foo(fixed : Boolean) extends Module {
//  val io = IO(new Bundle {
//    val in = Input(new FixedOrFloat(fixed))
//    val out = Output(new FixedOrFloat(fixed))
//  })
//  io.out := io.in
//}
//
//println((new ChiselStage).emitVerilog(new Foo(false)))
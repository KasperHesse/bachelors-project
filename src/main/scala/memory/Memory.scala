package memory

import chisel3._

class MemoryIO extends Bundle {

}

class Memory extends Module {
  val io = IO(new OnChipMemoryIO)


}

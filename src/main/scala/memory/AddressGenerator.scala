package memory

import chisel3._
import chisel3.util.{Decoupled, MuxLookup, RegEnable, is, switch}
import utils.Config._
import execution.StypeBaseAddress._
import execution._

/**
 * I/O ports for the address generator module
 */
class AddressGeneratorIO extends Bundle {
  val in = Flipped(Decoupled(new AddressGenProducerIO))
  val mem = Decoupled(new AddressGenMemoryIO)
}

/**
 * This module is used to generate addresses into the on-chip memory module.
 * An encoded base address is decoded inside the module, and the given index offset are added onto the decoded address.
 * The 8 decoded addresses are transmitted to the memory stage.
 * The given indices are reorderered such that the address ending in x000 is output at position 0, the address ending in x001 at position 1, etc.
 *
 *
 * @param pipe Whether ready/valid signals should be transmitted directly through the module (purely combinational module)
 *                   or values should be registered in the middle. //TODO currently not implemented
 *
 * @note It is up to the user/previous hardware to ensure that all 8 indices are mutually exclusive (do not try to access the same memory banks)
 */
class AddressGenerator(pipe: Boolean = false) extends Module {
  val io = IO(new AddressGeneratorIO)

  // --- MODULES ---
  val vectorOrderer = Module(new VectorOrderer)

  // --- REGISTERS ---
  /** Ready signal to producer */
  val readyInternal = WireDefault(true.B)
  /** Valid signal to consumer */
  val validInternal = if(pipe) RegInit(false.B) else WireDefault(false.B)
  val in = if(pipe) RegEnable(io.in.bits, io.in.valid && readyInternal) else io.in.bits


//   --- SIGNALS AND WIRES ---
  /** Decoder vec used to translate base address + indices to global addresses */
  val addrDecode = Wire(Vec(NUM_MEMORY_LOCS, UInt(MEM_ADDR_WIDTH.W)))
  for(i <- addrDecode.indices) {
    addrDecode(i) := AddressDecode.mapping(i).U
  }
  /** Vector holding global addreses, not yet ordered */
  val addresses = Wire(Vec(NUM_MEMORY_BANKS, UInt(MEM_ADDR_WIDTH.W)))

  /** Decoded base address */
  val baseAddr = addrDecode(in.baseAddr.asUInt())


  // --- LOGIC ---
  //Generate global addresses
  addresses := VecInit(in.indices.map(a => a + baseAddr))

  //Reorder / map them to correct locations
  vectorOrderer.io.addressIn := addresses
  vectorOrderer.io.validsIn := in.validIndices

  //Output them
  io.mem.bits.addr := vectorOrderer.io.addressOut
  io.mem.bits.validAddress := vectorOrderer.io.validsOut

  //Ready/valid handshake
  //Not outputting valid data, prod is not valid => valid=false
  //Not outputting valid data, prod is valid and cons is ready => valid=true
  //Not outputting valid, prod is valid, cons is not ready => valid=true
  //Outputting valid data, prod is valid and cons is ready => valid=true
  //Outputting valid data, prod is valid and cons is not ready => valid=true
  //Outputting valid data, prod is not valid and cons is ready => valid=false
  //Outputting valid data, prod is not valid, cons is not ready => valid=false
  if(pipe) {
    //valid logic
    when(!validInternal && io.in.valid) { //Not outputting valid data, but prod is valid
      validInternal := true.B
    } .elsewhen(validInternal && !io.in.valid) { //Outputting data, but prod does not have valid data
      validInternal := false.B
    }
    //ready logic
    when(!validInternal) { //When not outputting data, we're always ready
      readyInternal := true.B
    } .otherwise { //When outputting data, only ready if consumer is ready
      readyInternal := io.mem.ready
    }
  } else {
    validInternal := io.in.valid
    readyInternal := io.mem.ready
  }

  io.in.ready := readyInternal
  io.mem.valid := validInternal
}

/**
 * Performs a pseudo-ordering of vectors by the 3 LSB of their values.
 * If two values with the same 3 LSB are given, the one further "up" the vector is given priority.
 * Eg. if addressIn(1) = 0010 and addressIn(5) = 1010, then addressOut(1) will be 1010.
 * (eg if address(0)=x000(valid) and address(1)=x000(!valid), address(0) should be prioritized
 */
class VectorOrderer extends Module {
  val io = IO(new Bundle {
    val addressIn = Input(Vec(NUM_MEMORY_BANKS, UInt(MEM_ADDR_WIDTH.W)))
    val validsIn = Input(Vec(NUM_MEMORY_BANKS, Bool()))
    val addressOut = Output(Vec(NUM_MEMORY_BANKS, UInt(MEM_ADDR_WIDTH.W)))
    val validsOut = Output(Vec(NUM_MEMORY_BANKS, Bool()))
  })

  //Super-duper fugly scala/chisel-mix to generate output
  io.addressOut := io.addressIn
  io.validsOut := WireDefault(VecInit(Seq.fill(NUM_MEMORY_BANKS)(false.B)))

  for(i <- 0 until NUM_MEMORY_BANKS) {
    switch(io.addressIn(i)(2,0)) {
      is(0.U) { when(io.validsIn(i)) {
        io.addressOut(0) := io.addressIn(i)
        io.validsOut(0) := io.validsIn(i)
        }
      }
      is(1.U) { when(io.validsIn(i)) {
        io.addressOut(1) := io.addressIn(i)
        io.validsOut(1) := io.validsIn(i)
        }
      }
      is(2.U) { when(io.validsIn(i)) {
        io.addressOut(2) := io.addressIn(i)
        io.validsOut(2) := io.validsIn(i)
        }
      }
      is(3.U) { when(io.validsIn(i)) {
        io.addressOut(3) := io.addressIn(i)
        io.validsOut(3) := io.validsIn(i)
        }
      }
      is(4.U) { when(io.validsIn(i)) {
        io.addressOut(4) := io.addressIn(i)
        io.validsOut(4) := io.validsIn(i)
        }
      }
      is(5.U) { when(io.validsIn(i)) {
        io.addressOut(5) := io.addressIn(i)
        io.validsOut(5) := io.validsIn(i)
        }
      }
      is(6.U) { when(io.validsIn(i)) {
        io.addressOut(6) := io.addressIn(i)
        io.validsOut(6) := io.validsIn(i)
        }
      }
      is(7.U) { when(io.validsIn(i)) {
        io.addressOut(7) := io.addressIn(i)
        io.validsOut(7) := io.validsIn(i)
        }
      }
    }
  }
}

/**
 * A helper object used for address decoding. Contains a globally accessible value 'mapping' which contains the mapping
 * between the literal values of Stype modifiers and the base address in memory at which they start.
 * All memory addresses are offset such that the first element of that array starts at an address which is a multiple of
 * NUM_MEMORY_BANKS, such that the first element is always stored in bank 0, the next in bank 1, etc.
 */
object AddressDecode {
  //Defining a mapping to make the chisel code more navigable
  val mapping = scala.collection.mutable.Map[Int, Int]()
  mapping += (0 -> 0) //base address for X
  mapping += (1 -> (mapping(0) + NELEMSIZE)) //base address for XPHYS
  mapping += (2 -> (mapping(1) + NELEMSIZE)) //Base address for XNEW
  mapping += (3 -> (mapping(2) + NELEMSIZE)) //base address for DC
  mapping += (4 -> (mapping(3) + NELEMSIZE)) //base address for DV
  mapping += (5 -> (mapping(4) + NELEMSIZE)) //Base address for F
  mapping += (6 -> (mapping(5) + NDOFSIZE)) //Base address for U
  mapping += (7 -> (mapping(6) + NDOFSIZE)) //Base address for R
  mapping += (8 -> (mapping(7) + NDOFSIZE)) //Base address for Z
  mapping += (9 -> (mapping(8) + NDOFSIZE)) //Base address for P
  mapping += (10 -> (mapping(9) + NDOFSIZE)) //Base addrss for Q
  mapping += (11 -> (mapping(10) + NDOFSIZE)) //Base address for INVD
  mapping += (12 -> (mapping(11) + NDOFSIZE)) //Base address for TMP
}

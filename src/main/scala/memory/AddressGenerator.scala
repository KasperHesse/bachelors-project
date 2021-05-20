package memory

import chisel3._
import chisel3.util.{Decoupled, MuxLookup, is, switch}
import utils.Config._
import pipeline.StypeBaseAddress._
import pipeline._

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
 * @param registered Whether ready/valid signals should be transmitted directly through the module (purely combinational module)
 *                   or values should be registered in the middle. //TODO currently not implemented
 *
 * @note It is up to the user/previous hardware to ensure that all 8 indices are mutually exclusive (do not try to access the same memory banks)
 */
class AddressGenerator(registered: Boolean = false) extends Module {
  val io = IO(new AddressGeneratorIO)
  val in = io.in.bits //Easier shorthand access

  // --- MODULES ---
  val vectorOrderer = Module(new VectorOrderer)

  // --- REGISTERS ---

//   --- SIGNALS AND WIRES ---
  /** Decoder vec used to translate base address + indices to global addresses */
  val addrDecode = Wire(Vec(14, UInt(MEM_ADDR_WIDTH.W)))
  for(i <- addrDecode.indices) {
    addrDecode(i) := AddressDecode.mapping(i).U
  }
  /** Vector holding global addreses, not yet ordered */
  val addresses = Wire(Vec(NUM_MEMORY_BANKS, UInt(MEM_ADDR_WIDTH.W)))

  /** Decoded base address */
//  val baseAddr = MuxLookup(io.in.bits.baseAddr.asUInt(), 0.U, AddressDecode.getMapping())
  val baseAddr = addrDecode(io.in.bits.baseAddr.asUInt())


  // --- LOGIC ---
  //Generate global addresses
//  addresses := VecInit(in.indices.map(a => a + addrDecode(in.baseAddr.asUInt())))
  addresses := VecInit(in.indices.map(a => a + baseAddr))

  //Reorder / map them to correct locations
  vectorOrderer.io.addressIn := addresses
  vectorOrderer.io.validsIn := in.validIndices

  //Output them
  io.mem.bits.addr := vectorOrderer.io.addressOut
  io.mem.bits.validAddress := vectorOrderer.io.validsOut

  io.in.ready := io.mem.ready
  io.mem.valid := io.in.valid

  io.mem.bits.we := false.B
}

/**
 * Performs a pseudo-ordering of vectors by the 3 LSB of their values.
 * If two values with the same 3 LSB are given, the one further "up" the vector is given priority.
 * Eg. if addressIn(1) = 0010 and addressIn(5) = 1010, then addressOut(1) will be 1010.
 * //TODO must only sort addresses if the corresponding index is valid
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
//      is(1.U) {io.addressOut(1) := io.addressIn(i); io.validsOut(1) := io.validsIn(i)}
//      is(2.U) {io.addressOut(2) := io.addressIn(i); io.validsOut(2) := io.validsIn(i)}
//      is(3.U) {io.addressOut(3) := io.addressIn(i); io.validsOut(3) := io.validsIn(i)}
//      is(4.U) {io.addressOut(4) := io.addressIn(i); io.validsOut(4) := io.validsIn(i)}
//      is(5.U) {io.addressOut(5) := io.addressIn(i); io.validsOut(5) := io.validsIn(i)}
//      is(6.U) {io.addressOut(6) := io.addressIn(i); io.validsOut(6) := io.validsIn(i)}
//      is(7.U) {io.addressOut(7) := io.addressIn(i); io.validsOut(7) := io.validsIn(i)}
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
  /** NELEM padded to smallest multiple of NUM_MEMORY_BANKS >= NELEM */
  val NELEM_PAD = if(NELEM % NUM_MEMORY_BANKS == 0) NELEM else (NELEM/NUM_MEMORY_BANKS+1)*NELEM
  /** NDOF padded to smallest multiple of NUM_MEMORY_BANKS >= NDOF */
  val NDOF_PAD = if(NDOF % NUM_MEMORY_BANKS == 0) NDOF else (NDOF/NUM_MEMORY_BANKS+1)*NUM_MEMORY_BANKS
  /** ELEMS_PER_VSLOT padded to smallest multiple of NUM_MEMORY_BANKS >= ELEMS_PER_VSLOT */
 val ELEMS_PER_VSLOT_PAD = if(ELEMS_PER_VSLOT % NUM_MEMORY_BANKS == 0) ELEMS_PER_VSLOT else (ELEMS_PER_VSLOT/NUM_MEMORY_BANKS+1)*NUM_MEMORY_BANKS
  //Defining the width of each memory region
  val KE_WIDTH = ELEMS_PER_VSLOT_PAD
  val X_WIDTH = NELEM_PAD
  val XPHYS_WIDTH = NELEM_PAD
  val XNEW_WIDTH = NELEM_PAD
  val DC_WIDTH = NELEM_PAD
  val DV_WIDTH = NELEM_PAD
  val F_WIDTH = NDOF_PAD
  val U_WIDTH = NDOF_PAD
  val R_WIDTH = NDOF_PAD
  val Z_WIDTH = NDOF_PAD
  val P_WIDTH = NDOF_PAD
  val Q_WIDTH = NDOF_PAD
  val INVD_WIDTH = NDOF_PAD
  val TMP_WIDTH = NDOF_PAD

  //Defining a mapping to make the chisel code more navigable
  val mapping = scala.collection.mutable.Map[Int, Int]()
  mapping += (0 -> 0)
  mapping += (1 -> (mapping(0) + KE_WIDTH))
  mapping += (2 -> (mapping(1) + X_WIDTH))
  mapping += (3 -> (mapping(2) + XPHYS_WIDTH))
  mapping += (4 -> (mapping(3) + XNEW_WIDTH))
  mapping += (5 -> (mapping(4) + DC_WIDTH))
  mapping += (6 -> (mapping(5) + DV_WIDTH))
  mapping += (7 -> (mapping(6) + F_WIDTH))
  mapping += (8 -> (mapping(7) + U_WIDTH))
  mapping += (9 -> (mapping(8) + R_WIDTH))
  mapping += (10 -> (mapping(9) + Z_WIDTH))
  mapping += (11 -> (mapping(10) + P_WIDTH))
  mapping += (12 -> (mapping(11) + Q_WIDTH))
  mapping += (13 -> (mapping(12) + INVD_WIDTH))
//
//  val map2 = scala.collection.mutable.Map[StypeBaseAddress.Type, Int]()
//  map2 += (KE -> 0)
//  map2 += (X -> (map2(KE) + KE_WIDTH))
//  map2 += (XPHYS -> (map2(X) + X_WIDTH))
//  map2 += (XNEW -> (map2(XPHYS) + XPHYS_WIDTH))
//  map2 += (DC -> (map2(XNEW) + XNEW_WIDTH))
//  map2 += (DV -> (map2(DC) + DC_WIDTH))
//  map2 += (F -> (map2(DV) + DV_WIDTH))
//  map2 += (U -> (map2(F) + F_WIDTH))
//  map2 += (R -> (map2(U) + U_WIDTH))
//  map2 += (Z -> (map2(R)+ R_WIDTH))
//  map2 += (P -> (map2(Z) + Z_WIDTH))
//  map2 += (Q -> (map2(P) + P_WIDTH))
//  map2 += (INVD -> (map2(Q) + Q_WIDTH))
//  map2 += (TMP -> (map2(INVD) + INVD_WIDTH))
//
//  //Map to a seq for use in MuxLookup
//  private val map3 = map2.map(a => a._1.asUInt() -> a._2.U)
//
//  def getMapping(): Seq[(UInt, UInt)] = {
//    map3.toMap.toSeq
//  }
}

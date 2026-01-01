package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._
import riscv_inorder.CoreConstants._

case class DCacheReq(
  addr: UInt,
  data: UInt,
  size: HWEnum[MemWidth],
  signed: Bool,
  is_load: Bool,
  is_store: Bool,
  ldq_idx: UInt,
  stq_idx: UInt,
  rob_idx: UInt,
  br_mask: UInt,
) extends Bundle[DCacheReq]

object DCacheReq:
  def apply(p: CoreParams): DCacheReq =
    DCacheReq(
      addr = UInt(p.paddrBits.W),
      data = UInt(p.xlenBits.W),
      size = HWEnum(MemWidth),
      signed = Bool(),
      is_load = Bool(),
      is_store = Bool(),
      ldq_idx = UInt(p.ldqIdxBits.W),
      stq_idx = UInt(p.stqIdxBits.W),
      rob_idx = UInt(p.robIdxBits.W),
      br_mask = UInt(p.branchTagBits.W),
    )

case class DCacheResp(
  data: UInt,
  ldq_idx: UInt,
) extends Bundle[DCacheResp]

object DCacheResp:
  def apply(p: CoreParams): DCacheResp =
    DCacheResp(
      data = UInt(p.xlenBits.W),
      ldq_idx = UInt(p.ldqIdxBits.W),
    )

case class LSUDCacheIO(
  req: Vec[Decoupled[DCacheReq]],
  s1_kill: Vec[Bool],
  resp: Vec[Valid[DCacheResp]],
  nack: Vec[Valid[DCacheReq]],
  store_ack: Vec[Valid[DCacheReq]],
  ll_resp: Decoupled[DCacheResp],
) extends Bundle[LSUDCacheIO]

object LSUDCacheIO:
  def apply(p: CoreParams): LSUDCacheIO =
    LSUDCacheIO(
      req = Vec.fill(p.lsuIssueWidth)(Decoupled(DCacheReq(p))),
      s1_kill = Output(Vec.fill(p.lsuIssueWidth)(Bool())),
      resp = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheResp(p)))),
      nack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      store_ack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      ll_resp = Flipped(Decoupled(DCacheResp(p))),
    )

enum MSHRState:
  case Invalid,       // MSHR not allocated
       TagRead,       // Reading victim tag/data for potential writeback
       WaitEvict,     // Waiting for writeback to complete
       RefillReq,     // Sending refill request to memory
       RefillWait,    // Waiting for refill data
       RefillResp,    // Processing refill response
       WriteCache,    // Writing refilled data to cache
       Replay         // Replaying request to cache pipeline

case class MSHREntry(
  valid: Bool,
  state: HWEnum[MSHRState],

  req: DCacheReq,

  set_idx: UInt,
  tag: UInt,
  way_en: OneHot,

  victim_tag: UInt,
  victim_dirty: Bool,
  victim_valid: Bool,

  refill_data: UInt,
) extends Bundle[MSHREntry]

object MSHREntry:
  def apply(p: CoreParams): MSHREntry =
    val dc = p.dc
    val idxBits = log2Ceil(dc.nSets)
    val tagBits = p.paddrBits - idxBits - log2Ceil(dc.cacheLineBytes)

    MSHREntry(
      valid = Bool(),
      state = HWEnum(MSHRState),
      req = DCacheReq(p),
      set_idx = UInt(idxBits.W),
      tag = UInt(tagBits.W),
      way_en = OneHot(dc.nWays.W),
      victim_tag = UInt(tagBits.W),
      victim_dirty = Bool(),
      victim_valid = Bool(),
      refill_data = UInt((dc.cacheLineBytes * 8).W),
    )

case class L1Metadata(
  tag: UInt,
  valid: Bool,
  dirty: Bool,
) extends Bundle[L1Metadata]

object L1Metadata:
  def apply(p: CoreParams): L1Metadata =
    val dc = p.dc
    val idxBits = log2Ceil(dc.nSets)
    val tagBits = p.paddrBits - idxBits - log2Ceil(dc.cacheLineBytes)

    L1Metadata(
      tag = UInt(tagBits.W),
      valid = Bool(),
      dirty = Bool(),
    )

case class DCacheIO(
  lsu: LSUDCacheIO,
  mem: MagicMemIf,
) extends Bundle[DCacheIO]

object DCacheIO:
  def apply(p: CoreParams): DCacheIO =
    DCacheIO(
      lsu = Flipped(LSUDCacheIO(p)),
      mem = MagicMemIf(p),
    )

// ============================================================================
// Non-Blocking DCache
// - No coherency
// - No store-to-load forwarding
// - Duplicated arrays instead of banked
// ============================================================================
class NonBlockingDCache(p: CoreParams) extends Module:
  given Module = this

  val io = IO(DCacheIO(p))

  val dc = p.dc
  val nSets = dc.nSets
  val nWays = dc.nWays
  val nMSHRs = dc.mshrs
  val lineBytes = dc.cacheLineBytes
  val lineBits = lineBytes * 8
  val lsuWidth = p.lsuIssueWidth

  val idxBits = log2Ceil(nSets)
  val offsetBits = log2Ceil(lineBytes)
  val tagBits = p.paddrBits - idxBits - offsetBits

  body {
    import MSHRState._

    def getSetIdx(addr: UInt): UInt = addr(offsetBits + idxBits - 1, offsetBits)
    def getTag(addr: UInt): UInt = addr(p.paddrBits - 1, offsetBits + idxBits)
    def getOffset(addr: UInt): UInt = addr(offsetBits - 1, 0)
    def blockAlign(addr: UInt): UInt = Cat(Seq(addr(p.paddrBits - 1, offsetBits), 0.U(offsetBits.W)))

    val meta_arrays = Seq.fill(lsuWidth)(
      Seq.fill(nWays)(
        SRAM(L1Metadata(p), nSets)
            (reads = 1, writes = 1, readwrites = 0, masked = true)
      ))

    // Data SRAMs: one SRAM per port per way
    val data_arrays = Seq.fill(lsuWidth)(
      Seq.fill(nWays)(
        SRAM(UInt(lineBits.W), nSets)
            (reads = 1, writes = 1, readwrites = 0, masked = true)
      ))

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    def getReplacementWay(): UInt = {
      lfsr(log2Ceil(nWays) - 1, 0)
    }

    val s0_valid = Wire(Vec.fill(lsuWidth)(Bool()))
    val s0_req = Wire(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s0_set_idx = Wire(Vec.fill(lsuWidth)(UInt(idxBits.W)))

    for (w <- 0 until lsuWidth) {
      s0_valid(w) := io.lsu.req(w).valid
      s0_req(w) := io.lsu.req(w).bits
      s0_set_idx(w) := getSetIdx(io.lsu.req(w).bits.addr)

      // FIXME: mshr full
      io.lsu.req(w).ready := true.B

      for (way <- 0 until nWays) {
        meta_arrays(w)(way).readPorts(0).read(s0_set_idx(w), s0_valid(w))
        data_arrays(w)(way).readPorts(0).read(s0_set_idx(w), s0_valid(w))
      }
    }

    val s1_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s1_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))

    val s2_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s2_tag_hit_way = Reg(Vec.fill(lsuWidth)(OneHot(nWays.W)))
    val s2_tag_hit = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_data_array = Reg(Vec.fill(lsuWidth)(Vec.fill(nWays)(UInt(lineBits.W))))

    val s3_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s3_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s3_way = Reg(Vec.fill(lsuWidth)(OneHot(nWays.W)))
    val s3_set_idx = Reg(Vec.fill(lsuWidth)(UInt(idxBits.W)))

    val s1_meta_array = Wire(Vec.fill(lsuWidth)(Vec.fill(nWays)(L1Metadata(p))))
    val s1_data_array = Wire(Vec.fill(lsuWidth)(Vec.fill(nWays)(UInt(lineBits.W))))
    for (w <- 0 until lsuWidth) {
      for (x <- 0 until nWays) {
        s1_meta_array(w)(x) := meta_arrays(w)(x).readPorts(0).data
        s1_data_array(w)(x) := data_arrays(w)(x).readPorts(0).data
      }
    }

    for (w <- 0 until lsuWidth) {
      s1_valid(w) := s0_valid(w)
      s1_req(w) := s0_req(w)

      val tag_hit_oh = Cat(s1_meta_array(w).map(m => {
        s1_valid(w) &&
        m.valid &&
        m.tag === getTag(s1_req(w).addr)
      }).reverse)

      val tag_hit = tag_hit_oh =/= 0.U
      val tag_hit_way = tag_hit_oh.asOH

      when (!io.lsu.s1_kill(w)) {
        s2_valid(w) := s1_valid(w)
        s2_req(w) := s1_req(w)
        s2_tag_hit_way(w) := tag_hit_way
        s2_tag_hit(w) := tag_hit
        s2_data_array(w) := s1_data_array(w)
      }.otherwise {
        s2_valid(w) := false.B
      }
    }

    for (w <- 0 until lsuWidth) {
      val s2_data = MuxOneHot(s2_tag_hit_way(w), s2_data_array(w).elems)

      val offset = getOffset(s2_req(w).addr)
      val byte_offset = offset(log2Ceil(lineBytes) - 1, 0)
      val shifted_data = s2_data >> (byte_offset << 3.U)

      val sign_bit_byte = Mux(s2_req(w).signed, shifted_data(7), false.B).asUInt
      val sign_bit_half = Mux(s2_req(w).signed, shifted_data(15), false.B).asUInt
      val sign_bit_word = Mux(s2_req(w).signed, shifted_data(31), false.B).asUInt

      val load_data_byte = Cat(Seq(Seq.fill(p.xlenBits - 8)(sign_bit_byte), Seq(shifted_data(7, 0))).flatten)
      val load_data_half = Cat(Seq(Seq.fill(p.xlenBits - 16)(sign_bit_half), Seq(shifted_data(15, 0))).flatten)
      val load_data_word = Cat(Seq(Seq.fill(p.xlenBits - 32)(sign_bit_word), Seq(shifted_data(31, 0))).flatten)
      val load_data_dword = shifted_data(p.xlenBits - 1, 0)

      val load_data = Mux(s2_req(w).size === MemWidth.B.EN, load_data_byte,
                      Mux(s2_req(w).size === MemWidth.H.EN, load_data_half,
                      Mux(s2_req(w).size === MemWidth.W.EN, load_data_word,
                          load_data_dword)))

      io.lsu.resp(w).valid := s2_valid(w) && s2_req(w).is_load && s2_tag_hit(w)
      io.lsu.resp(w).bits.data := load_data
      io.lsu.resp(w).bits.ldq_idx := s2_req(w).ldq_idx

      io.lsu.store_ack(w).valid := s2_valid(w) && s2_req(w).is_store && s2_tag_hit(w)
      io.lsu.store_ack(w).bits := s2_req(w)

      io.lsu.nack(w).valid := s2_valid(w) && !s2_tag_hit(w)
      io.lsu.nack(w).bits := s2_req(w)
    }

    for (w <- 0 until lsuWidth) {
      s3_valid(w) := false.B
      for (w <- 0 until lsuWidth) {
        when (s2_valid(w) && s2_req(w).is_store && s2_tag_hit(w)) {
          s3_valid(w) := true.B
          s3_req(w) := s2_req(w)
          s3_way(w) := s2_tag_hit_way(w)
          s3_set_idx(w) := getSetIdx(s2_req(w).addr)
        }
      }

      when (s3_valid(w)) {
        val offset = getOffset(s3_req(w).addr)
        val byte_offset = offset(log2Ceil(lineBytes) - 1, 0)

        val wmask = Wire(Vec.fill(lineBytes)(Bool()))
        for (i <- 0 until lineBytes) {
          wmask(i) := false.B
        }

        val num_bytes = Mux(s3_req(w).size === MemWidth.B.EN, 1.U,
                          Mux(s3_req(w).size === MemWidth.H.EN, 2.U,
                            Mux(s3_req(w).size === MemWidth.W.EN, 4.U, 8.U)))

        for (i <- 0 until lineBytes) {
          when (i.U >= byte_offset && i.U < (byte_offset + num_bytes)) {
            wmask(i) := true.B
          }
        }

        val write_data = s3_req(w).data << (byte_offset << 3.U)

        for (way <- 0 until nWays) {
          when (s3_way.asUInt(way).asBool) {
            for (w <- 0 until lsuWidth) {
              data_arrays(w)(way).writePorts(0).write(s3_set_idx(w), write_data, wmask)

              val new_meta = Wire(L1Metadata(p))
              new_meta.tag := getTag(s3_req(w).addr)
              new_meta.valid := true.B
              new_meta.dirty := true.B
              meta_arrays(w)(way).writePorts(0).write(s3_set_idx(w), new_meta)
            }
          }
        }
      }
    }

    io.lsu.ll_resp.valid := false.B
    io.lsu.ll_resp.bits := DontCare

    io.mem <> DontCare

    when (reset.asBool) {
      s1_valid.foreach(_ := false.B)
      s2_valid.foreach(_ := false.B)
    }

  }

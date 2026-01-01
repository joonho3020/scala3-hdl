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
      }
    }

    val s1_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s1_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s2_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))

    val s1_meta_array = Wire(Vec.fill(lsuWidth)(Vec.fill(nWays)(L1Metadata(p))))
    for (w <- 0 until lsuWidth) {
      for (x <- 0 until nWays) {
        s1_meta_array(w)(x) := meta_arrays(w)(x).readPorts(0).data
      }
    }

    for (w <- 0 until lsuWidth) {
      s1_valid(w) := s0_valid(w)
      s1_req(w) := s0_req(w)

      s2_valid(w) := s1_valid(w)
      s2_req(w) := s1_req(w)

      val tag_hit_oh = Cat(s1_meta_array(w).map(m => {
        s1_valid(w) &&
        m.valid &&
        m.tag === getTag(s1_req(w).addr)
      }).reverse)

      val tag_hit = tag_hit_oh =/= 0.U
      val tag_hit_way = PriorityEncoder(tag_hit_oh)
    }

    // Placeholder outputs for now
    for (w <- 0 until lsuWidth) {
      io.lsu.req(w).ready := false.B
      io.lsu.resp(w).valid := false.B
      io.lsu.resp(w).bits := DontCare
      io.lsu.nack(w).valid := false.B
      io.lsu.nack(w).bits := DontCare
      io.lsu.store_ack(w).valid := false.B
      io.lsu.store_ack(w).bits := DontCare
    }
    io.lsu.ll_resp.valid := false.B
    io.lsu.ll_resp.bits := DontCare

    io.mem <> DontCare

  }

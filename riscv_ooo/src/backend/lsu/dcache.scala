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
  store_nack: Vec[Valid[DCacheReq]],
  store_ack: Vec[Valid[DCacheReq]],
  ll_resp: Decoupled[DCacheResp],
) extends Bundle[LSUDCacheIO]

object LSUDCacheIO:
  def apply(p: CoreParams): LSUDCacheIO =
    LSUDCacheIO(
      req = Vec.fill(p.lsuIssueWidth)(Decoupled(DCacheReq(p))),
      s1_kill = Output(Vec.fill(p.lsuIssueWidth)(Bool())),
      resp = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheResp(p)))),
      store_nack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      store_ack = Input(Vec.fill(p.lsuIssueWidth)(Valid(DCacheReq(p)))),
      ll_resp = Flipped(Decoupled(DCacheResp(p))),
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

case class TagWriteReq(
  set_idx: UInt,
  way_en: OneHot,
  meta: L1Metadata
) extends Bundle[TagWriteReq]

object TagWriteReq:
  def apply(p: CoreParams): TagWriteReq =
    val dc = p.dc
    val idxBits = log2Ceil(dc.nSets)
    TagWriteReq(
      set_idx = UInt(idxBits.W),
      way_en = OneHot(dc.nWays.W),
      meta = L1Metadata(p)
    )

case class DataWriteReq(
  set_idx: UInt,
  way_en: OneHot,
  data: UInt,
  wmask: Vec[Bool]
) extends Bundle[DataWriteReq]

object DataWriteReq:
  def apply(p: CoreParams): DataWriteReq =
    val dc = p.dc
    val idxBits = log2Ceil(dc.nSets)
    val lineBytes = dc.cacheLineBytes
    DataWriteReq(
      set_idx = UInt(idxBits.W),
      way_en = OneHot(dc.nWays.W),
      data = UInt((lineBytes * 8).W),
      wmask = Vec.fill(lineBytes)(Bool())
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
    def getSetIdx(addr: UInt): UInt = addr(offsetBits + idxBits - 1, offsetBits)
    def getTag(addr: UInt): UInt = addr(p.paddrBits - 1, offsetBits + idxBits)
    def getOffset(addr: UInt): UInt = addr(offsetBits - 1, 0)
    def blockAlign(addr: UInt): UInt = Cat(Seq(addr(p.paddrBits - 1, offsetBits), 0.U(offsetBits.W)))

    val meta_arrays = Seq.fill(lsuWidth)(
      Seq.fill(nWays)(
        SRAM(L1Metadata(p), nSets)
            (reads = 1, writes = 1, readwrites = 0, masked = true)
      ))

    val data_arrays = Seq.fill(lsuWidth)(
      Seq.fill(nWays)(
        SRAM(UInt(lineBits.W), nSets)
            (reads = 1, writes = 1, readwrites = 0, masked = true)
      ))

    val mshrFile = Module(new DCacheMSHRFile(p))
    io.mem.req <> mshrFile.io.mem.req
    mshrFile.io.mem.resp <> io.mem.resp

    // Tag write arbiter: MSHR refills + S3 stores
    // Priority: MSHR (input 0) > S3 stores (inputs 1..lsuWidth)
    val tagWriteArb = Module(new Arbiter(TagWriteReq(p), 1 + lsuWidth))
    tagWriteArb.io.out.ready := true.B  // SRAM writes always complete immediately

    // Data write arbiter: MSHR refills + S3 stores
    val dataWriteArb = Module(new Arbiter(DataWriteReq(p), 1 + lsuWidth))
    dataWriteArb.io.out.ready := true.B

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    def getReplacementWay(): UInt = {
      lfsr(log2Ceil(nWays) - 1, 0)
    }

    val s0_valid = Wire(Vec.fill(lsuWidth)(Bool()))
    val s0_req = Wire(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s0_set_idx = Wire(Vec.fill(lsuWidth)(UInt(idxBits.W)))
    val s0_replay = Wire(Vec.fill(lsuWidth)(Bool()))

    val replay_valid = mshrFile.io.replay.valid
    val replay_req = mshrFile.io.replay.bits.req
    val replay_mshr_idx = mshrFile.io.replay.bits.idx

    for (w <- 0 until lsuWidth) {
      // Port 0 gives priority to replay requests
      if (w == 0) {
        when (replay_valid) {
          s0_valid(w) := true.B
          s0_req(w) := replay_req
          s0_replay(w) := true.B
          io.lsu.req(w).ready := false.B
        }.otherwise {
          s0_valid(w) := io.lsu.req(w).valid
          s0_req(w) := io.lsu.req(w).bits
          s0_replay(w) := false.B
          io.lsu.req(w).ready := true.B
        }
      } else {
        s0_valid(w) := io.lsu.req(w).valid
        s0_req(w) := io.lsu.req(w).bits
        s0_replay(w) := false.B
        io.lsu.req(w).ready := true.B
      }

      s0_set_idx(w) := getSetIdx(s0_req(w).addr)

      for (way <- 0 until nWays) {
        meta_arrays(w)(way).readPorts(0).read(s0_set_idx(w), s0_valid(w))
        data_arrays(w)(way).readPorts(0).read(s0_set_idx(w), s0_valid(w))
      }
    }

    val s1_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s1_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s1_replay = Reg(Vec.fill(lsuWidth)(Bool()))
    val s1_replay_mshr_idx = Reg(Vec.fill(lsuWidth)(UInt(log2Ceil(nMSHRs).W)))

    val s2_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s2_tag_hit_way = Reg(Vec.fill(lsuWidth)(OneHot(nWays.W)))
    val s2_tag_hit = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_meta_array = Reg(Vec.fill(lsuWidth)(Vec.fill(nWays)(L1Metadata(p))))
    val s2_data_array = Reg(Vec.fill(lsuWidth)(Vec.fill(nWays)(UInt(lineBits.W))))
    val s2_replay = Reg(Vec.fill(lsuWidth)(Bool()))
    val s2_replay_mshr_idx = Reg(Vec.fill(lsuWidth)(UInt(log2Ceil(nMSHRs).W)))

    val s3_valid = Reg(Vec.fill(lsuWidth)(Bool()))
    val s3_req   = Reg(Vec.fill(lsuWidth)(DCacheReq(p)))
    val s3_way = Reg(Vec.fill(lsuWidth)(OneHot(nWays.W)))
    val s3_set_idx = Reg(Vec.fill(lsuWidth)(UInt(idxBits.W)))
    val s3_tag_hit = Reg(Vec.fill(lsuWidth)(Bool()))

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
      s1_replay(w) := s0_replay(w)
      when (s0_replay(w)) {
        s1_replay_mshr_idx(w) := replay_mshr_idx
      }

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
        s2_meta_array(w) := s1_meta_array(w)
        s2_data_array(w) := s1_data_array(w)
        s2_replay(w) := s1_replay(w)
        s2_replay_mshr_idx(w) := s1_replay_mshr_idx(w)
      }.otherwise {
        s2_valid(w) := false.B
      }
    }

    // Happy path, hits
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
    }

    // Sad path, misses
    for (w <- 0 until lsuWidth) {
      val victim_way = getReplacementWay()
      val victim_way_oh = UIntToOH(victim_way)
      val victim_meta = MuxOneHot(victim_way_oh, s2_meta_array(w).elems)
      val victim_data = MuxOneHot(victim_way_oh, s2_data_array(w).elems)

      mshrFile.io.alloc(w).valid := s2_valid(w) && !s2_tag_hit(w)
      mshrFile.io.alloc(w).bits.req := s2_req(w)
      mshrFile.io.alloc(w).bits.set_idx := getSetIdx(s2_req(w).addr)
      mshrFile.io.alloc(w).bits.tag := getTag(s2_req(w).addr)
      mshrFile.io.alloc(w).bits.way_en := victim_way_oh
      mshrFile.io.alloc(w).bits.victim_tag := victim_meta.tag
      mshrFile.io.alloc(w).bits.victim_dirty := victim_meta.dirty
      mshrFile.io.alloc(w).bits.victim_valid := victim_meta.valid
      mshrFile.io.alloc(w).bits.victim_data := victim_data

      io.lsu.store_nack(w).valid := s2_valid(w) && !s2_tag_hit(w) && !mshrFile.io.can_allocate(w)
      io.lsu.store_nack(w).bits  := s2_req(w)
    }

    // S2 -> S3: Advance store hits to S3 for tag/data write arbitration
    for (w <- 0 until lsuWidth) {
      when (s2_valid(w) && s2_req(w).is_store && s2_tag_hit(w)) {
        s3_valid(w) := true.B
        s3_req(w) := s2_req(w)
        s3_way(w) := s2_tag_hit_way(w)
        s3_set_idx(w) := getSetIdx(s2_req(w).addr)
        s3_tag_hit(w) := true.B
      }.otherwise {
        s3_valid(w) := false.B
      }
    }

    // S3: Tag write arbitration
    // Input 0: MSHR refills (highest priority)
    // Inputs 1..lsuWidth: S3 store hits
    tagWriteArb.io.in(0).valid := mshrFile.io.tag_write.valid
    tagWriteArb.io.in(0).bits := mshrFile.io.tag_write.bits
    mshrFile.io.tag_write.ready := tagWriteArb.io.in(0).ready

    for (w <- 0 until lsuWidth) {
      val s3_store = s3_valid(w) && s3_tag_hit(w)
      tagWriteArb.io.in(w + 1).valid := s3_store
      tagWriteArb.io.in(w + 1).bits.set_idx := s3_set_idx(w)
      tagWriteArb.io.in(w + 1).bits.way_en := s3_way(w)
      tagWriteArb.io.in(w + 1).bits.meta.tag := getTag(s3_req(w).addr)
      tagWriteArb.io.in(w + 1).bits.meta.valid := true.B
      tagWriteArb.io.in(w + 1).bits.meta.dirty := true.B
    }

    // Perform winning tag write (broadcast to all tag arrays)
    when (tagWriteArb.io.out.fire) {
      for (w <- 0 until lsuWidth) {
        for (way <- 0 until nWays) {
          when (tagWriteArb.io.out.bits.way_en.asUInt(way).asBool) {
            meta_arrays(w)(way).writePorts(0).write(
              tagWriteArb.io.out.bits.set_idx,
              tagWriteArb.io.out.bits.meta
            )
          }
        }
      }
    }

    // S3: Data write arbitration
    dataWriteArb.io.in(0).valid := mshrFile.io.data_write.valid
    dataWriteArb.io.in(0).bits := mshrFile.io.data_write.bits
    mshrFile.io.data_write.ready := dataWriteArb.io.in(0).ready

    for (w <- 0 until lsuWidth) {
      val s3_store = s3_valid(w) && s3_tag_hit(w)

      val offset = getOffset(s3_req(w).addr)
      val byte_offset = offset(log2Ceil(lineBytes) - 1, 0)
      val write_data = s3_req(w).data << (byte_offset << 3.U)

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

      dataWriteArb.io.in(w + 1).valid := s3_store
      dataWriteArb.io.in(w + 1).bits.set_idx := s3_set_idx(w)
      dataWriteArb.io.in(w + 1).bits.way_en := s3_way(w)
      dataWriteArb.io.in(w + 1).bits.data := write_data
      dataWriteArb.io.in(w + 1).bits.wmask := wmask
    }

    // Perform winning data write (to specific port's data arrays)
    when (dataWriteArb.io.out.fire) {
      for (w <- 0 until lsuWidth) {
        for (way <- 0 until nWays) {
          when (dataWriteArb.io.out.bits.way_en.asUInt(way).asBool) {
            data_arrays(w)(way).writePorts(0).write(
              dataWriteArb.io.out.bits.set_idx,
              dataWriteArb.io.out.bits.data,
              dataWriteArb.io.out.bits.wmask
            )
          }
        }
      }
    }

    // S3: Store ack/nack generation
    for (w <- 0 until lsuWidth) {
      val s3_store = s3_valid(w) && s3_tag_hit(w)
      val s3_won_tag_arb = tagWriteArb.io.in(w + 1).ready
      val s3_won_data_arb = dataWriteArb.io.in(w + 1).ready

      Assert(!(s3_won_tag_arb ^ s3_won_data_arb),
        "Tag and data arbitration going different ways")

      // Ack if won both arbitrations
      io.lsu.store_ack(w).valid := s3_store && s3_won_tag_arb && s3_won_data_arb
      io.lsu.store_ack(w).bits := s3_req(w)

      // Nack if wanted to write but lost arbitration
      // NOTE: We keep the S2 nack for misses, add S3 nack for arbitration conflicts
      val s3_nack = s3_store && (!s3_won_tag_arb || !s3_won_data_arb)
      when (s3_nack) {
        io.lsu.store_nack(w).valid := true.B
        io.lsu.store_nack(w).bits := s3_req(w)
      }
    }

    io.lsu.ll_resp.valid := false.B
    io.lsu.ll_resp.bits := DontCare
    mshrFile.io.free.valid := false.B
    mshrFile.io.free.bits := DontCare

    when (s2_replay(0) && s2_tag_hit(0)) {
      val s2_data = MuxOneHot(s2_tag_hit_way(0), s2_data_array(0).elems)

      val offset = getOffset(s2_req(0).addr)
      val byte_offset = offset(log2Ceil(lineBytes) - 1, 0)
      val shifted_data = s2_data >> (byte_offset << 3.U)

      val sign_bit_byte = Mux(s2_req(0).signed, shifted_data(7), false.B).asUInt
      val sign_bit_half = Mux(s2_req(0).signed, shifted_data(15), false.B).asUInt
      val sign_bit_word = Mux(s2_req(0).signed, shifted_data(31), false.B).asUInt

      val load_data_byte = Cat(Seq(Seq.fill(p.xlenBits - 8)(sign_bit_byte), Seq(shifted_data(7, 0))).flatten)
      val load_data_half = Cat(Seq(Seq.fill(p.xlenBits - 16)(sign_bit_half), Seq(shifted_data(15, 0))).flatten)
      val load_data_word = Cat(Seq(Seq.fill(p.xlenBits - 32)(sign_bit_word), Seq(shifted_data(31, 0))).flatten)
      val load_data_dword = shifted_data(p.xlenBits - 1, 0)

      val load_data = Mux(s2_req(0).size === MemWidth.B.EN, load_data_byte,
                      Mux(s2_req(0).size === MemWidth.H.EN, load_data_half,
                      Mux(s2_req(0).size === MemWidth.W.EN, load_data_word,
                          load_data_dword)))

      io.lsu.ll_resp.valid := true.B
      io.lsu.ll_resp.bits.data := load_data
      io.lsu.ll_resp.bits.ldq_idx := s2_req(0).ldq_idx

      mshrFile.io.free.valid := true.B
      mshrFile.io.free.bits := s2_replay_mshr_idx(0)
    }

    when (reset.asBool) {
      s1_valid.foreach(_ := false.B)
      s2_valid.foreach(_ := false.B)
      s3_valid.foreach(_ := false.B)
      s1_replay.foreach(_ := false.B)
      s2_replay.foreach(_ := false.B)
    }
  }

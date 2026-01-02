package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

enum MSHRState:
  case Invalid, TagRead, WaitEvict, RefillReq, RefillWait, RefillResp, WriteCache, Replay

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

case class DCacheMSHRAllocReq(
  req: DCacheReq,
  set_idx: UInt,
  tag: UInt,
  way_en: OneHot,
  victim_tag: UInt,
  victim_dirty: Bool,
  victim_valid: Bool,
  victim_data: UInt,
) extends Bundle[DCacheMSHRAllocReq]

object DCacheMSHRAllocReq:
  def apply(p: CoreParams): DCacheMSHRAllocReq =
    val dc = p.dc
    val idxBits = log2Ceil(dc.nSets)
    val offsetBits = log2Ceil(dc.cacheLineBytes)
    val tagBits = p.paddrBits - idxBits - offsetBits
    val lineBits = dc.cacheLineBytes * 8
    DCacheMSHRAllocReq(
      req = DCacheReq(p),
      set_idx = UInt(idxBits.W),
      tag = UInt(tagBits.W),
      way_en = OneHot(dc.nWays.W),
      victim_tag = UInt(tagBits.W),
      victim_dirty = Bool(),
      victim_valid = Bool(),
      victim_data = UInt(lineBits.W),
    )

case class DCacheMSHRReplay(
  req: DCacheReq,
  idx: UInt,
) extends Bundle[DCacheMSHRReplay]

object DCacheMSHRReplay:
  def apply(p: CoreParams): DCacheMSHRReplay =
    val nMSHRs = p.dc.mshrs
    DCacheMSHRReplay(
      req = DCacheReq(p),
      idx = UInt(log2Ceil(nMSHRs).W),
    )

case class DCacheMSHRFileIO(
  alloc: Vec[Valid[DCacheMSHRAllocReq]],
  can_allocate: Vec[Bool],
  free: Valid[UInt],
  mem: MagicMemIf,
  tag_write: Decoupled[TagWriteReq],
  data_write: Decoupled[DataWriteReq],
  replay: Valid[DCacheMSHRReplay],
) extends Bundle[DCacheMSHRFileIO]

object DCacheMSHRFileIO:
  def apply(p: CoreParams): DCacheMSHRFileIO =
    val lsuWidth = p.lsuIssueWidth
    val nMSHRs = p.dc.mshrs
    DCacheMSHRFileIO(
      alloc = Input(Vec.fill(lsuWidth)(Valid(DCacheMSHRAllocReq(p)))),
      can_allocate = Output(Vec.fill(lsuWidth)(Bool())),
      free = Input(Valid(UInt(log2Ceil(nMSHRs).W))),
      mem = MagicMemIf(p),
      tag_write = Decoupled(TagWriteReq(p)),
      data_write = Decoupled(DataWriteReq(p)),
      replay = Output(Valid(DCacheMSHRReplay(p))),
    )

class DCacheMSHRFile(p: CoreParams) extends Module:
  given Module = this

  val io = IO(DCacheMSHRFileIO(p))

  val dc = p.dc
  val nMSHRs = dc.mshrs
  val lsuWidth = p.lsuIssueWidth
  val lineBytes = dc.cacheLineBytes
  val offsetBits = log2Ceil(lineBytes)

  def blockAlign(addr: UInt): UInt =
    Cat(Seq(addr(p.paddrBits - 1, offsetBits), 0.U(offsetBits.W)))

  body {
    val mshrs = Reg(Vec.fill(nMSHRs)(MSHREntry(p)))

    val mshr_alloc_idx = Wire(Vec.fill(lsuWidth)(UInt(log2Ceil(nMSHRs).W)))
    val can_allocate_mshr = Wire(Vec.fill(lsuWidth)(Bool()))

    var free_mshrs = Seq.fill(lsuWidth)(Wire(Vec.fill(nMSHRs)(Bool())))
    free_mshrs(0).zip(mshrs).foreach((f, m) => f := !m.valid)

    for (w <- 0 until lsuWidth) {
      can_allocate_mshr(w) := free_mshrs(w).reduce(_ || _)
      mshr_alloc_idx(w) := PriorityEncoder(Cat(free_mshrs(w).reverse))

      val allocate_mshr = io.alloc(w).valid && can_allocate_mshr(w)
      when (allocate_mshr) {
        val idx = mshr_alloc_idx(w)
        mshrs(idx).valid := true.B
        mshrs(idx).req := io.alloc(w).bits.req
        mshrs(idx).set_idx := io.alloc(w).bits.set_idx
        mshrs(idx).tag := io.alloc(w).bits.tag
        mshrs(idx).way_en := io.alloc(w).bits.way_en
        mshrs(idx).victim_tag := io.alloc(w).bits.victim_tag
        mshrs(idx).victim_dirty := io.alloc(w).bits.victim_dirty
        mshrs(idx).victim_valid := io.alloc(w).bits.victim_valid
        mshrs(idx).refill_data := io.alloc(w).bits.victim_data
        when (io.alloc(w).bits.victim_valid && io.alloc(w).bits.victim_dirty) {
          mshrs(idx).state := MSHRState.WaitEvict.EN
        }.otherwise {
          mshrs(idx).state := MSHRState.RefillReq.EN
        }
      }

      if w < lsuWidth-1 then {
        for (i <- 0 until nMSHRs) {
          free_mshrs(w+1)(i) := Mux(allocate_mshr && i.U === mshr_alloc_idx(w), false.B, free_mshrs(w)(i))
        }
      }
    }

    io.can_allocate.zip(can_allocate_mshr).foreach((o, i) => o := i)

    val writebackArb = Module(new RRArbiter(MagicMemReq(p), nMSHRs))
    val refillReqArb = Module(new RRArbiter(MagicMemReq(p), nMSHRs))

    for (i <- 0 until nMSHRs) {
      writebackArb.io.in(i).valid := mshrs(i).valid && mshrs(i).state === MSHRState.WaitEvict.EN
      writebackArb.io.in(i).bits.addr := blockAlign(Cat(Seq(mshrs(i).victim_tag, mshrs(i).set_idx, 0.U(offsetBits.W))))
      writebackArb.io.in(i).bits.tpe := MagicMemMsg.Write.EN
      writebackArb.io.in(i).bits.data := Splice(mshrs(i).refill_data, 32)
      writebackArb.io.in(i).bits.mask := Fill(p.memLineBytes, true.B)
      writebackArb.io.in(i).bits.tag := i.U

      refillReqArb.io.in(i).valid := mshrs(i).valid && mshrs(i).state === MSHRState.RefillReq.EN
      refillReqArb.io.in(i).bits.addr := blockAlign(Cat(Seq(mshrs(i).tag, mshrs(i).set_idx, 0.U(offsetBits.W))))
      refillReqArb.io.in(i).bits.tpe := MagicMemMsg.Read.EN
      refillReqArb.io.in(i).bits.data := DontCare
      refillReqArb.io.in(i).bits.mask := Fill(p.memLineBytes, true.B)
      refillReqArb.io.in(i).bits.tag := i.U
    }

    writebackArb.io.out.ready := io.mem.req.ready
    refillReqArb.io.out.ready := io.mem.req.ready && !writebackArb.io.out.valid

    io.mem.req.valid := writebackArb.io.out.valid || refillReqArb.io.out.valid
    io.mem.req.bits := Mux(writebackArb.io.out.valid, writebackArb.io.out.bits, refillReqArb.io.out.bits)

    when (writebackArb.io.out.fire) {
      mshrs(writebackArb.io.chosen).state := MSHRState.RefillReq.EN
    }

    when (refillReqArb.io.out.fire) {
      mshrs(refillReqArb.io.chosen).state := MSHRState.RefillWait.EN
    }

    when (io.mem.resp.valid) {
      val resp_data = Cat(io.mem.resp.bits.lineWords.reverse)
      val mshr_idx = io.mem.resp.bits.tag(log2Ceil(nMSHRs) - 1, 0)
      when (mshrs(mshr_idx).valid && mshrs(mshr_idx).state === MSHRState.RefillWait.EN) {
        mshrs(mshr_idx).refill_data := resp_data
        mshrs(mshr_idx).state := MSHRState.WriteCache.EN
      }
    }

    type writeBundleIf = (tag: TagWriteReq, data: DataWriteReq)
    val writeBundle = Bundle((
      tag = TagWriteReq(p),
      data = DataWriteReq(p)
    ))

    val writeCacheArb = Module(new RRArbiter(writeBundle, nMSHRs))
    for (i <- 0 until nMSHRs) {
      writeCacheArb.io.in(i).valid := mshrs(i).valid && mshrs(i).state === MSHRState.WriteCache.EN
      writeCacheArb.io.in(i).bits.tag.set_idx := mshrs(i).set_idx
      writeCacheArb.io.in(i).bits.tag.way_en := mshrs(i).way_en
      writeCacheArb.io.in(i).bits.tag.meta.tag := mshrs(i).tag
      writeCacheArb.io.in(i).bits.tag.meta.valid := true.B
      writeCacheArb.io.in(i).bits.tag.meta.dirty := false.B
      writeCacheArb.io.in(i).bits.data.set_idx := mshrs(i).set_idx
      writeCacheArb.io.in(i).bits.data.way_en := mshrs(i).way_en
      writeCacheArb.io.in(i).bits.data.data := mshrs(i).refill_data
      for (b <- 0 until lineBytes) {
        writeCacheArb.io.in(i).bits.data.wmask(b) := true.B
      }
    }

    val writeCacheReady = io.tag_write.ready && io.data_write.ready
    writeCacheArb.io.out.ready := writeCacheReady

    io.tag_write.valid := writeCacheArb.io.out.valid
    io.tag_write.bits := DontCare
    io.data_write.valid := writeCacheArb.io.out.valid
    io.data_write.bits := DontCare

    when (writeCacheArb.io.out.valid) {
      io.tag_write.bits  := writeCacheArb.io.out.bits.tag
      io.data_write.bits := writeCacheArb.io.out.bits.data
    }

    when (writeCacheArb.io.out.fire) {
      mshrs(writeCacheArb.io.chosen).state := MSHRState.Replay.EN
    }

    val replayArb = Module(new RRArbiter(DCacheMSHRReplay(p), nMSHRs))
    for (i <- 0 until nMSHRs) {
      replayArb.io.in(i).valid := mshrs(i).valid && mshrs(i).state === MSHRState.Replay.EN
      replayArb.io.in(i).bits.req := mshrs(i).req
      replayArb.io.in(i).bits.idx := i.U
    }

    replayArb.io.out.ready := true.B
    io.replay.valid := replayArb.io.out.valid
    io.replay.bits := replayArb.io.out.bits

    when (replayArb.io.out.fire) {
      mshrs(replayArb.io.chosen).state := MSHRState.Invalid.EN
    }

    when (io.free.valid) {
      mshrs(io.free.bits).valid := false.B
      mshrs(io.free.bits).state := MSHRState.Invalid.EN
    }

    when (reset.asBool) {
      for (i <- 0 until nMSHRs) {
        mshrs(i).valid := false.B
        mshrs(i).state := MSHRState.Invalid.EN
      }
    }
  }

package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._


case class BTBEntry(
  valid: Bool,
  tag:   UInt,
  target: UInt,
  is_ret: Bool,
  is_call: Bool,
) extends Bundle[BTBEntry]

object BTBEntry:
  def apply(pcBits: Int, entries: Int): BTBEntry =
    val btbIdxBits = log2Ceil(entries)

    new BTBEntry(
      valid = Bool(),
      tag   = UInt((pcBits - (btbIdxBits + 2)).W),
      target = UInt(pcBits.W),
      is_ret = Bool(),
      is_call = Bool(),
    )

case class BPUReq(
  pc: UInt
) extends Bundle[BPUReq]

object BPUReq:
  def apply(p: CoreParams): BPUReq =
    BPUReq(
      pc = UInt(p.pcBits.W)
    )

case class BPUResp(
  hit: Bool,
  taken: Bool,
  target: UInt,
  is_ret: Bool,
  uses_ras: Bool
) extends Bundle[BPUResp]

object BPUResp:
  def apply(p: CoreParams): BPUResp =
    BPUResp(
      hit = Bool(),
      taken = Bool(),
      target = UInt(p.pcBits.W),
      is_ret = Bool(),
      uses_ras = Bool()
    )

case class BPUUpdate(
  pc: UInt,
  target: UInt,
  taken: Bool,
  is_call: Bool,
  is_ret: Bool,
  cfi_mask: UInt,
  cfi_idx: UInt,
) extends Bundle[BPUUpdate]

object BPUUpdate:
  def apply(p: CoreParams): BPUUpdate =
    BPUUpdate(
      pc = UInt(p.pcBits.W),
      target = UInt(p.pcBits.W),
      taken = Bool(),
      is_call = Bool(),
      is_ret = Bool(),
      cfi_mask = UInt(p.coreWidth.W),
      cfi_idx = UInt(log2Ceil(p.coreWidth+1).W),
    )

case class RASSnapshot(
  ptr: UInt,
  top: UInt
) extends Bundle[RASSnapshot]

object RASSnapshot:
  def apply(p: CoreParams): RASSnapshot =
    RASSnapshot(
      ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      top = UInt(p.pcBits.W)
    )

case class BPURestoreIf(
  valid: Bool,
  pc: UInt,
  ghist: UInt,
  lhist: UInt,
  ras: RASSnapshot
) extends Bundle[BPURestoreIf]

object BPURestoreIf:
  def apply(p: CoreParams): BPURestoreIf =
    BPURestoreIf(
      valid = Bool(),
      pc = UInt(p.pcBits.W),
      ghist = UInt(p.bpu.ghistBits.W),
      lhist = UInt(p.bpu.lhistBits.W),
      ras   = RASSnapshot(p),
    )

case class BranchPredictorIO(
  req: Valid[BPUReq],
  resp: Vec[Valid[BPUResp]],
  bpu_update: Valid[BPUUpdate],
  ras_update: Valid[BPUUpdate],
  flush: Bool,
  ghist: UInt,
  lhist: UInt,
  ras_snapshot: RASSnapshot,
  restore: BPURestoreIf,
) extends Bundle[BranchPredictorIO]

object BranchPredictorIO:
  def apply(p: CoreParams): BranchPredictorIO =
    BranchPredictorIO(
      req = Input(Valid(BPUReq(p))),
      resp = Output(Vec.fill(p.coreWidth)(Valid(BPUResp(p)))),
      bpu_update = Flipped(Valid(BPUUpdate(p))),
      ras_update = Flipped(Valid(BPUUpdate(p))),
      flush = Input(Bool()),
      ghist = Output(UInt(p.bpu.ghistBits.W)),
      lhist = Output(UInt(p.bpu.lhistBits.W)),
      ras_snapshot = Output(RASSnapshot(p)),
      restore = Input(BPURestoreIf(p)),
    )

// TODO: Bank BTB, BHT memory structures
class BranchPredictor(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  class RAS(nras: Int):
    private val count = RegInit(0.U(log2Up(nras+1).W))
    private val ptr   = RegInit(0.U(log2Up(nras)  .W))
    private val stack = Reg(Vec.fill(nras)(UInt(p.pcBits.W)))

    dontTouch(count)
    dontTouch(ptr)
    dontTouch(stack)

    def top: UInt =
      val top_idx = Mux(ptr === 0.U, (nras - 1).U, ptr - 1.U)
      stack(top_idx)
    def empty: Bool = count === 0.U

    def get_idx: UInt = ptr

    def get_snapshot: RASSnapshot =
      val snap = Wire(RASSnapshot(p))
      snap.ptr := ptr
      snap.top := top
      snap

    def restore(snapshot: RASSnapshot): Unit =
      ptr := snapshot.ptr
      val top_idx = Mux(snapshot.ptr === 0.U, (nras - 1).U, snapshot.ptr - 1.U)
      stack(top_idx) := snapshot.top

    def clear: Unit =
      count := 0.U
      ptr := 0.U

    def pop: Unit =
      when (!empty) {
        ptr   := Mux(ptr === 0.U, (nras - 1).U, ptr - 1.U)
        count := count - 1.U
      }

    def push(addr: UInt): Unit =
      when (count < nras.U) {
        stack(ptr) := addr
        ptr := Mux(ptr === (nras - 1).U, 0.U, ptr + 1.U)
        count := count + 1.U
      }

  class BTB(entries: Int):
    val btbIdxBits = log2Ceil(entries)

    private val btb = Reg(Vec.fill(entries)(BTBEntry(pcBits = p.pcBits, entries = entries)))
    dontTouch(btb)

    def lookup(pc: UInt): (Bool, BTBEntry) =
      val btb_idx = if entries == 1 then 0.U else pc(btbIdxBits + 1, 2)
      val tag_in = pc(p.pcBits - 1, btbIdxBits + 2)
      val entry = btb(btb_idx)
      val hit = entry.valid && (entry.tag === tag_in)
      (hit, entry)

    def update(pc: UInt, target_pc: UInt, is_ret: Bool, is_call: Bool): Unit =
      val btb_idx = if entries == 1 then 0.U else pc(btbIdxBits + 1, 2)
      val btb_tag = pc(p.pcBits - 1, btbIdxBits + 2)
      val entry = btb(btb_idx)
      entry.valid   := true.B
      entry.tag     := btb_tag
      entry.target  := target_pc
      entry.is_ret  := is_ret
      entry.is_call := is_call

    def clear: Unit =
      for (i <- 0 until entries) {
        btb(i).valid := false.B
      }

  class GHT(entries: Int, ghistBits: Int):
    val phtIdxBits = log2Ceil(entries)
    private val pht = Reg(Vec.fill(entries)(UInt(2.W)))

    def hash(pc: UInt, ghist: UInt): UInt =
      val pc_bits = if entries == 1 then 0.U else pc(phtIdxBits + 1, 2)
      val ghist_folded = ghist(phtIdxBits - 1, 0)
      pc_bits ^ ghist_folded

    def lookup(pc: UInt, ghist: UInt): Bool =
      val idx = hash(pc, ghist)
      pht(idx)(1).asBool

    def update(pc: UInt, ghist: UInt, taken: Bool): Unit =
      val update_idx = hash(pc, ghist)
      val counter = pht(update_idx)
      val next = Wire(UInt(2.W))
      next := counter
      when (taken && counter =/= 3.U) { next := counter + 1.U }
      when (!taken && counter =/= 0.U) { next := counter - 1.U }
      pht(update_idx) := next

    def clear: Unit =
      pht.foreach(_ := 1.U)

  class GHR(ghistBits: Int):
    private val ghist = RegInit(0.U(ghistBits.W))

    def read: UInt = ghist

    def update(taken: Bool): Unit =
      ghist := (ghist << 1) | taken.asUInt

    def restore(value: UInt): Unit =
      ghist := value

    def clear: Unit =
      ghist := 0.U

  class LHT(entries: Int, lhistBits: Int):
    val lhtIdxBits = log2Ceil(entries)
    private val lht = Reg(Vec.fill(entries)(UInt(lhistBits.W)))

    def lookup(pc: UInt): UInt =
      val idx = if entries == 1 then 0.U else pc(lhtIdxBits + 1, 2)
      lht(idx)

    def update(pc: UInt, taken: Bool): Unit =
      val idx = if entries == 1 then 0.U else pc(lhtIdxBits + 1, 2)
      lht(idx) := (lht(idx) << 1) | taken.asUInt

    def restore(pc: UInt, lhist: UInt): Unit =
      val idx = if entries == 1 then 0.U else pc(lhtIdxBits + 1, 2)
      lht(idx) := lhist

    def clear: Unit =
      lht.foreach(_ := 0.U)

  class LocalPHT(entries: Int, lhistBits: Int):
    val phtIdxBits = log2Ceil(entries)
    private val pht = Reg(Vec.fill(entries)(UInt(2.W)))

    def lookup(lhist: UInt): Bool =
      val idx = lhist(phtIdxBits - 1, 0)
      pht(idx)(1).asBool

    def update(lhist: UInt, taken: Bool): Unit =
      val idx = lhist(phtIdxBits - 1, 0)
      val counter = pht(idx)
      val next = Wire(UInt(2.W))
      next := counter
      when (taken && counter =/= 3.U) { next := counter + 1.U }
      when (!taken && counter =/= 0.U) { next := counter - 1.U }
      pht(idx) := next

    def clear: Unit =
      pht.foreach(_ := 1.U)

  class MetaPredictor(entries: Int, ghistBits: Int):
    val metaIdxBits = log2Ceil(entries)
    private val meta_table = Reg(Vec.fill(entries)(UInt(2.W)))

    def hash(pc: UInt, ghist: UInt): UInt =
      val pc_bits = if entries == 1 then 0.U else pc(metaIdxBits + 1, 2)
      val ghist_folded = ghist(metaIdxBits - 1, 0)
      pc_bits ^ ghist_folded

    def select(pc: UInt, ghist: UInt): Bool =
      val idx = hash(pc, ghist)
      meta_table(idx)(1).asBool

    def update(pc: UInt, ghist: UInt, global_correct: Bool, local_correct: Bool): Unit =
      when (global_correct =/= local_correct) {
        val idx = hash(pc, ghist)
        val counter = meta_table(idx)
        val next = Wire(UInt(2.W))
        next := counter
        when (global_correct && counter =/= 3.U) { next := counter + 1.U }
        when (local_correct && counter =/= 0.U) { next := counter - 1.U }
        meta_table(idx) := next
      }

    def clear: Unit =
      meta_table.foreach(_ := 1.U)

  val io = IO(BranchPredictorIO(p))

  body {
    val btbEntries = p.bpu.btbEntries
    val rasEntries = p.bpu.rasEntries
    val ghtEntries = p.bpu.ghtEntries
    val ghistBits = p.bpu.ghistBits

    val btbIdxBits = log2Ceil(btbEntries)

    require(btbEntries > 0)
    require(rasEntries > 0)
    require(p.pcBits > btbIdxBits + 2)
    require(p.pcBits > 2)

    val ras = new RAS(rasEntries)
    val btb = new BTB(btbEntries)
    val ght = new GHT(ghtEntries, ghistBits)
    val ghr = new GHR(ghistBits)
    val lht = new LHT(p.bpu.lhtEntries, p.bpu.lhistBits)
    val local_pht = new LocalPHT(p.bpu.localPhtEntries, p.bpu.lhistBits)
    val meta = new MetaPredictor(p.bpu.metaEntries, p.bpu.ghistBits)

    io.ghist := ghr.read
    io.lhist := lht.lookup(io.req.bits.pc)
    io.ras_snapshot := ras.get_snapshot

    for (i <- 0 until p.coreWidth) {
      io.resp(i).valid := io.req.valid
      io.resp(i).bits  := DontCare

      val pc = io.req.bits.pc + (4 * i).U

      val global_pred = ght.lookup(pc, ghr.read)

      val lhist = lht.lookup(pc)
      val local_pred = local_pht.lookup(lhist)

      val use_global = meta.select(pc, ghr.read)
      val pred_taken = Mux(use_global, global_pred, local_pred)

      val (btb_hit, btb_entry) = btb.lookup(pc)
      val use_ras = btb_entry.is_ret && !ras.empty
      val predicted_target = Mux(use_ras, ras.top, btb_entry.target)
      val predicted_taken = btb_hit && (use_ras || pred_taken)

      io.resp(i).bits.hit := btb_hit
      io.resp(i).bits.taken := predicted_taken
      io.resp(i).bits.target := predicted_target
      io.resp(i).bits.is_ret := btb_entry.is_ret
      io.resp(i).bits.uses_ras := use_ras
    }

    when (io.bpu_update.valid) {
      val taken = io.bpu_update.bits.taken
      val pc = io.bpu_update.bits.pc

      val ghist_for_update = ghr.read
      val lhist_for_update = lht.lookup(pc)

      val global_pred = ght.lookup(pc, ghist_for_update)
      val local_pred = local_pht.lookup(lhist_for_update)
      val global_correct = (global_pred === taken)
      val local_correct = (local_pred === taken)

      ght.update(pc, ghist_for_update, taken)
      local_pht.update(lhist_for_update, taken)
      meta.update(pc, ghist_for_update, global_correct, local_correct)

      ghr.update(taken)
      lht.update(pc, taken)

      when (taken) {
        btb.update(
          io.bpu_update.bits.pc + (io.bpu_update.bits.cfi_idx << 2),
          io.bpu_update.bits.target,
          io.bpu_update.bits.is_ret,
          io.bpu_update.bits.is_call)
      }
    }

    when (io.ras_update.valid) {
      when (io.ras_update.bits.is_call) {
        ras.push(io.ras_update.bits.pc + p.instBytes.U)
      } .elsewhen (io.ras_update.bits.is_ret) {
        ras.pop
      }
    }

    when (io.restore.valid) {
      ghr.restore(io.restore.ghist)
      ras.restore(io.restore.ras)
      lht.restore(io.restore.pc, io.restore.lhist)
    }

    when (reset.asBool || io.flush) {
      btb.clear
      ras.clear
      ght.clear
      ghr.clear
      lht.clear
      local_pht.clear
      meta.clear
    }
  }

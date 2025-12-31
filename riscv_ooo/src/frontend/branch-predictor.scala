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
  pc: UInt,
  cfi_mask: UInt,
  cfi_idx: UInt,
  cfi_taken: Bool
) extends Bundle[BPUReq]

object BPUReq:
  def apply(p: CoreParams): BPUReq =
    BPUReq(
      pc = UInt(p.pcBits.W),
      cfi_mask = UInt(p.coreWidth.W),
      cfi_idx = UInt(log2Ceil(p.coreWidth + 1).W),
      cfi_taken = Bool()
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
  ghist: UInt,
  lhist: UInt,
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
      ghist = UInt(p.bpu.ghistBits.W),
      lhist = UInt(p.bpu.lhistBits.W),
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

  class LocalPHT(entries: Int, lhistBits: Int):
    val phtIdxBits = log2Ceil(entries)
    private val pht = Reg(Vec.fill(entries)(UInt(2.W)))

    def lookup(lhist: UInt): Bool =
      val idx = lhist(phtIdxBits - 1, 0)
      pht(idx)(1).asBool

    def update(lhist: UInt, taken: Bool): Unit =
      val idx = lhist(phtIdxBits - 1, 0)
      val counter = pht(idx)
      val next = WireInit(counter)
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
    val local_pht = new LocalPHT(p.bpu.localPhtEntries, p.bpu.lhistBits)
    val meta = new MetaPredictor(p.bpu.metaEntries, p.bpu.ghistBits)

    val ghist = RegInit(0.U(p.bpu.ghistBits.W))

    // LHT (Local History Table) - stores history per PC
    val lhtEntries = p.bpu.lhtEntries
    val lhtIdxBits = log2Ceil(lhtEntries)
    val lht = Reg(Vec.fill(lhtEntries)(UInt(p.bpu.lhistBits.W)))

    io.ghist := ghist
    io.lhist := {
      val idx = if lhtEntries == 1 then 0.U else io.req.bits.pc(lhtIdxBits + 1, 2)
      lht(idx)
    }
    io.ras_snapshot := ras.get_snapshot

    // Track speculative history within fetch packet
    // Create a chain of speculative ghist values, one per fetch position
    val spec_ghist = Wire(Vec.fill(p.coreWidth + 1)(UInt(p.bpu.ghistBits.W)))
    val spec_lhist = Wire(Vec.fill(p.coreWidth + 1)(Vec.fill(lhtEntries)(UInt(p.bpu.lhistBits.W))))

    spec_ghist(0) := ghist
    spec_lhist(0) := lht

    for (i <- 0 until p.coreWidth) {
      val pc = io.req.bits.pc + (4 * i).U

      val current_ghist = spec_ghist(i)
      val current_lhist = spec_lhist(i)

      val global_pred = ght.lookup(pc, current_ghist)

      val lht_idx = if lhtEntries == 1 then 0.U else pc(lhtIdxBits + 1, 2)
      val lhist = current_lhist(lht_idx)
      val local_pred = local_pht.lookup(lhist)

      val use_global = meta.select(pc, current_ghist)
      val pred_taken = Mux(use_global, global_pred, local_pred)

      val (btb_hit, btb_entry) = btb.lookup(pc)
      val use_ras = btb_entry.is_ret && !ras.empty
      val predicted_target = Mux(use_ras, ras.top, btb_entry.target)
      val predicted_taken = btb_hit && (use_ras || pred_taken)

      io.resp(i).valid := io.req.valid
      io.resp(i).bits.hit := btb_hit
      io.resp(i).bits.taken := predicted_taken
      io.resp(i).bits.target := predicted_target
      io.resp(i).bits.is_ret := btb_entry.is_ret
      io.resp(i).bits.uses_ras := use_ras

      // Speculatively update history based on BTB hit and prediction
      val is_cfi = btb_hit
      val taken = predicted_taken

      // Speculatively update history for next position in fetch packet
      val next_ghist = Wire(UInt(p.bpu.ghistBits.W))
      val next_lhist = Wire(Vec.fill(lhtEntries)(UInt(p.bpu.lhistBits.W)))

      next_ghist := Mux(is_cfi, (current_ghist << 1) | taken.asUInt, current_ghist)
      next_lhist := current_lhist
      when (is_cfi) {
        next_lhist(lht_idx) := (current_lhist(lht_idx) << 1) | taken.asUInt
      }

      spec_ghist(i + 1) := next_ghist
      spec_lhist(i + 1) := next_lhist
    }

    // Find the first taken branch to know where to stop updating history/RAS
    val taken_mask = Wire(Vec.fill(p.coreWidth)(Bool()))
    for (i <- 0 until p.coreWidth) {
      taken_mask(i) := io.resp(i).valid && io.resp(i).bits.taken
    }

    // Priority encode to find first taken position
    val has_taken = taken_mask.reduce(_ || _)
    val first_taken_idx = PriorityEncoder(Cat(taken_mask.reverse))
    val commit_idx = Mux(has_taken, first_taken_idx + 1.U, p.coreWidth.U)

    // Commit speculative updates only up to first taken branch
    when (io.req.valid) {
      ghist := spec_ghist(commit_idx)
      lht := spec_lhist(commit_idx)

      // Update RAS only for the first taken call/return
      when (has_taken) {
        val taken_pc = io.req.bits.pc + (first_taken_idx << 2)
        val taken_resp = io.resp(first_taken_idx).bits
        when (taken_resp.hit) {
          // Use BTB entry for the taken position
          val (_, taken_btb_entry) = btb.lookup(taken_pc)
          when (taken_btb_entry.is_call) {
            ras.push(taken_pc + p.instBytes.U)
          } .elsewhen (taken_btb_entry.is_ret) {
            ras.pop
          }
        }
      }
    }

    when (io.bpu_update.valid) {
      val cfi_mask = io.bpu_update.bits.cfi_mask
      val cfi_idx = io.bpu_update.bits.cfi_idx
      val cfi_taken = io.bpu_update.bits.taken
      val base_pc = io.bpu_update.bits.pc

      val update_ghist = Wire(Vec.fill(p.coreWidth + 1)(UInt(p.bpu.ghistBits.W)))
      update_ghist(0) := io.bpu_update.bits.ghist

      // Process all CFIs in the fetch packet, respecting cfi_mask
      for (i <- 0 until p.coreWidth) {
        val is_cfi = cfi_mask(i).asBool
        val cfi_pc = base_pc + (i << 2).U
        val current_ghist = update_ghist(i)

        // CFIs before cfi_idx are not-taken, CFI at cfi_idx uses cfi_taken
        val is_this_cfi_taken = (i.U === cfi_idx) && cfi_taken

        when (is_cfi) {
          val global_pred = ght.lookup(cfi_pc, current_ghist)

          val cfi_lhist = io.bpu_update.bits.lhist
          val local_pred = local_pht.lookup(cfi_lhist)

          val global_correct = (global_pred === is_this_cfi_taken)
          val local_correct = (local_pred === is_this_cfi_taken)

          // Update GHT, Local PHT, and Meta predictor for this CFI
          ght.update(cfi_pc, current_ghist, is_this_cfi_taken)
          local_pht.update(cfi_lhist, is_this_cfi_taken)
          meta.update(cfi_pc, current_ghist, global_correct, local_correct)

          // Update BTB when this CFI is taken
          when (is_this_cfi_taken) {
            btb.update(
              cfi_pc,
              io.bpu_update.bits.target,
              io.bpu_update.bits.is_ret,
              io.bpu_update.bits.is_call)
          }
        }

        // Update ghist for next position (only if this position had a CFI)
        update_ghist(i + 1) := Mux(is_cfi, (current_ghist << 1) | is_this_cfi_taken.asUInt, current_ghist)
      }
    }

    when (io.restore.valid) {
      ghist := io.restore.ghist
      ras.restore(io.restore.ras)
      val lht_idx = if lhtEntries == 1 then 0.U else io.restore.pc(lhtIdxBits + 1, 2)
      lht(lht_idx) := io.restore.lhist
    }

    when (reset.asBool || io.flush) {
      btb.clear
      ras.clear
      ght.clear
      ghist := 0.U
      lht.foreach(_ := 0.U)
      local_pht.clear
      meta.clear
    }

    dontTouch(io)
  }

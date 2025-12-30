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
  is_ret: Bool
) extends Bundle[BPUUpdate]

object BPUUpdate:
  def apply(p: CoreParams): BPUUpdate =
    BPUUpdate(
      pc = UInt(p.pcBits.W),
      target = UInt(p.pcBits.W),
      taken = Bool(),
      is_call = Bool(),
      is_ret = Bool()
    )

case class RASSnapshot(
  ras_ptr: UInt,
  ras_top: UInt
) extends Bundle[RASSnapshot]

object RASSnapshot:
  def apply(p: CoreParams): RASSnapshot =
    RASSnapshot(
      ras_ptr = UInt(log2Ceil(p.bpu.rasEntries + 1).W),
      ras_top = UInt(p.pcBits.W)
    )

case class BranchPredictorIO(
  req: Valid[BPUReq],
  resp: Vec[Valid[BPUResp]],
  bht_btb_update: Valid[BPUUpdate],
  ras_update: Valid[BPUUpdate],
  flush: Bool,
  ghist: UInt,
  ras_snapshot: RASSnapshot,
  restore_ghist: Valid[UInt],
  restore_ras: Valid[RASSnapshot],
) extends Bundle[BranchPredictorIO]

object BranchPredictorIO:
  def apply(p: CoreParams): BranchPredictorIO =
    BranchPredictorIO(
      req = Input(Valid(BPUReq(p))),
      resp = Output(Vec.fill(p.coreWidth)(Valid(BPUResp(p)))),
      bht_btb_update = Flipped(Valid(BPUUpdate(p))),
      ras_update = Flipped(Valid(BPUUpdate(p))),
      flush = Input(Bool()),
      ghist = Output(UInt(p.bpu.ghistBits.W)),
      ras_snapshot = Output(RASSnapshot(p)),
      restore_ghist = Flipped(Valid(UInt(p.bpu.ghistBits.W))),
      restore_ras = Flipped(Valid(RASSnapshot(p))),
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
      snap.ras_ptr := ptr
      snap.ras_top := top
      snap
    def restore(snapshot: RASSnapshot): Unit =
      ptr := snapshot.ras_ptr
      val top_idx = Mux(snapshot.ras_ptr === 0.U, (nras - 1).U, ptr - 1.U)
      stack(top_idx) := snapshot.ras_top
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

    io.ghist := ghr.read
    io.ras_snapshot := ras.get_snapshot

    for (i <- 0 until p.coreWidth) {
      io.resp(i).valid := io.req.valid
      io.resp(i).bits  := DontCare

      val pc = io.req.bits.pc + (4 * i).U
      val pred_taken = ght.lookup(pc, ghr.read)

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

    when (io.bht_btb_update.valid) {
      val taken = io.bht_btb_update.bits.taken
      val pc = io.bht_btb_update.bits.pc
      val ghist_for_update = ghr.read
      ght.update(pc, ghist_for_update, taken)
      ghr.update(taken)

      when (taken) {
        btb.update(
          io.bht_btb_update.bits.pc,
          io.bht_btb_update.bits.target,
          io.bht_btb_update.bits.is_ret,
          io.bht_btb_update.bits.is_call)
      }
    }

    when (io.ras_update.valid) {
      when (io.ras_update.bits.is_call) {
        ras.push(io.ras_update.bits.pc + p.instBytes.U)
      } .elsewhen (io.ras_update.bits.is_ret) {
        ras.pop
      }
    }

    when (io.restore_ghist.valid) {
      ghr.restore(io.restore_ghist.bits)
    }

    when (io.restore_ras.valid) {
      ras.restore(io.restore_ras.bits)
    }

    when (reset.asBool || io.flush) {
      btb.clear
      ras.clear
      ght.clear
      ghr.clear
    }
  }

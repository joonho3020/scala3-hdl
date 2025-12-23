package riscv_ooo

import hdl._

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

case class BranchPredictorIO(
  req: Valid[BPUReq],
  resp: Vec[Valid[BPUResp]],
  update: Valid[BPUUpdate],
  flush: Bool
) extends Bundle[BranchPredictorIO]

object BranchPredictorIO:
  def apply(p: CoreParams): BranchPredictorIO =
    BranchPredictorIO(
      req = Input(Valid(BPUReq(p))),
      resp = Output(Vec.fill(p.coreWidth)(Valid(BPUResp(p)))),
      update = Flipped(Valid(BPUUpdate(p))),
      flush = Input(Bool())
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

  class BHT(entries: Int):
    val bhtIdxBits = log2Ceil(entries)
    private val bht = Reg(Vec.fill(entries)(UInt(2.W)))

    def lookup(pc: UInt): Bool =
      val idx = if entries == 1 then 0.U else pc(bhtIdxBits + 1, 2)
      bht(idx)(1).asBool

    def update(pc: UInt, taken: Bool): Unit =
      val update_idx = if entries == 1 then 0.U else pc(bhtIdxBits + 1, 2)
      val counter = bht(update_idx)
      val next = Wire(UInt(2.W))
      next := counter
      when (taken && counter =/= 3.U) { next := counter + 1.U }
      when (!taken && counter =/= 0.U) { next := counter - 1.U }
      bht(update_idx) := next

    def clear: Unit =
      bht.foreach(_ := 1.U)

  val io = IO(BranchPredictorIO(p))

  body {
    val bhtEntries = p.bpu.bhtEntries
    val btbEntries = p.bpu.btbEntries
    val rasEntries = p.bpu.rasEntries

    val bhtIdxBits = log2Ceil(bhtEntries)
    val btbIdxBits = log2Ceil(btbEntries)

    require(bhtEntries > 0)
    require(btbEntries > 0)
    require(rasEntries > 0)
    require(p.pcBits > btbIdxBits + 2)
    require(p.pcBits > 2)

    val ras = new RAS(rasEntries)
    val btb = new BTB(btbEntries)
    val bht = new BHT(bhtEntries)

    for (i <- 0 until p.coreWidth) {
    }

// io.resp.valid := io.req.valid
// io.resp.bits.hit := false.B
// io.resp.bits.taken := false.B
// io.resp.bits.target := 0.U(p.pcBits.W)
// io.resp.bits.is_ret := false.B
// io.resp.bits.uses_ras := false.B

    for (i <- 0 until p.coreWidth) {
      io.resp(i).valid := io.req.valid
      io.resp(i).bits  := DontCare

      val pc = io.req.bits.pc + (4 * i).U
      val pred_taken = bht.lookup(pc)
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

    when (io.update.valid) {
      val taken = io.update.bits.taken
      bht.update(io.update.bits.pc, taken)

      when (taken) {
        btb.update(
          io.update.bits.pc,
          io.update.bits.target,
          io.update.bits.is_ret,
          io.update.bits.is_call)
      }

      when (io.update.bits.is_call) {
        ras.push(io.update.bits.pc + p.instBytes.U)
      } .elsewhen (io.update.bits.is_ret) {
        ras.pop
      }
    }

    when (reset.asBool || io.flush) {
      bht.clear
      btb.clear
      ras.clear
    }
  }

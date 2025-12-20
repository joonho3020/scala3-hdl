package riscv

import hdl._

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
  resp: Valid[BPUResp],
  update: Valid[BPUUpdate],
  flush: Bool
) extends Bundle[BranchPredictorIO]

object BranchPredictorIO:
  def apply(p: CoreParams): BranchPredictorIO =
    BranchPredictorIO(
      req = Input(Valid(BPUReq(p))),
      resp = Output(Valid(BPUResp(p))),
      update = Flipped(Valid(BPUUpdate(p))),
      flush = Input(Bool())
    )

class BranchPredictor(p: CoreParams) extends Module:
  given Module = this
  val io = IO(BranchPredictorIO(p))

  body {
    val bhtEntries = p.bpu.bhtEntries
    val btbEntries = p.bpu.btbEntries
    val rasEntries = p.bpu.rasEntries

    val bhtIdxBits = log2Ceil(bhtEntries)
    val btbIdxBits = log2Ceil(btbEntries)
    val rasIdxBits = log2Ceil(rasEntries max 2)

    require(bhtEntries > 0)
    require(btbEntries > 0)
    require(rasEntries > 0)
    require(p.pcBits > btbIdxBits + 2)
    require(p.pcBits > 2)

    val bht = Reg(Vec.fill(bhtEntries)(UInt(2.W)))

    val btb_valid = Reg(Vec.fill(btbEntries)(Bool()))
    val btb_tag = Reg(Vec.fill(btbEntries)(UInt((p.pcBits - (btbIdxBits + 2)).W)))
    val btb_target = Reg(Vec.fill(btbEntries)(UInt(p.pcBits.W)))
    val btb_is_ret = Reg(Vec.fill(btbEntries)(Bool()))
    val btb_is_call = Reg(Vec.fill(btbEntries)(Bool()))

    val ras = Reg(Vec.fill(rasEntries)(UInt(p.pcBits.W)))
    val ras_head = RegInit(0.U(rasIdxBits.W))
    val ras_depth = RegInit(0.U(log2Ceil(rasEntries + 1).W))

    val ras_empty = ras_depth === 0.U
    val ras_top = ras(ras_head)

    val ras_push = WireInit(false.B)
    val ras_pop = WireInit(false.B)
    val ras_push_addr = WireInit(0.U(p.pcBits.W))

    io.resp.valid := io.req.valid
    io.resp.bits.hit := false.B
    io.resp.bits.taken := false.B
    io.resp.bits.target := 0.U(p.pcBits.W)
    io.resp.bits.is_ret := false.B
    io.resp.bits.uses_ras := false.B

    val bht_idx = if bhtEntries == 1 then 0.U else io.req.bits.pc(bhtIdxBits + 1, 2)
    val btb_idx = if btbEntries == 1 then 0.U else io.req.bits.pc(btbIdxBits + 1, 2)

    val bht_state = bht(bht_idx)
    val bht_taken = bht_state(1).asBool

    val btb_tag_in = io.req.bits.pc(p.pcBits - 1, btbIdxBits + 2)
    val btb_hit = btb_valid(btb_idx) && btb_tag(btb_idx) === btb_tag_in

    val predicted_target = Mux(btb_is_ret(btb_idx) && !ras_empty, ras_top, btb_target(btb_idx))
    val predicted_taken = btb_hit && bht_taken

    when (io.req.valid) {
      io.resp.bits.hit := btb_hit
      io.resp.bits.taken := predicted_taken
      io.resp.bits.target := predicted_target
      io.resp.bits.is_ret := btb_is_ret(btb_idx)
      io.resp.bits.uses_ras := btb_is_ret(btb_idx) && !ras_empty
    }

    when (io.update.valid) {
      val taken = io.update.bits.taken
      val update_idx = if bhtEntries == 1 then 0.U else io.update.bits.pc(bhtIdxBits + 1, 2)
      val counter = bht(update_idx)
      val next = Wire(UInt(2.W))
      next := counter
      when (taken && counter =/= 3.U) { next := counter + 1.U }
      when (!taken && counter =/= 0.U) { next := counter - 1.U }
      bht(update_idx) := next

      val btb_update_idx = if btbEntries == 1 then 0.U else io.update.bits.pc(btbIdxBits + 1, 2)
      val btb_update_tag = io.update.bits.pc(p.pcBits - 1, btbIdxBits + 2)
      when (taken) {
        btb_valid(btb_update_idx) := true.B
        btb_tag(btb_update_idx) := btb_update_tag
        btb_target(btb_update_idx) := io.update.bits.target
        btb_is_ret(btb_update_idx) := io.update.bits.is_ret
        btb_is_call(btb_update_idx) := io.update.bits.is_call
      }

      ras_push := io.update.bits.is_call
      ras_pop := io.update.bits.is_ret
      ras_push_addr := io.update.bits.pc + p.instBytes.U
    }

    val ras_push_idx =
      Mux(ras_empty, 0.U, Mux(ras_head === (rasEntries - 1).U, 0.U, ras_head + 1.U))
    val ras_pop_idx = ras_head
    val ras_next_head =
      Mux(ras_pop_idx === 0.U, (rasEntries - 1).U, ras_pop_idx - 1.U)

    when (ras_push) {
      ras(ras_push_idx) := ras_push_addr
      ras_head := ras_push_idx
      when (ras_depth =/= rasEntries.U) { ras_depth := ras_depth + 1.U }
    }

    when (ras_pop && !ras_empty) {
      ras_head := ras_next_head
      ras_depth := ras_depth - 1.U
    }

    when (reset.asBool || io.flush) {
      for (i <- 0 until bhtEntries) { bht(i) := 1.U }
      for (i <- 0 until btbEntries) {
        btb_valid(i) := false.B
        btb_tag(i) := 0.U
        btb_target(i) := 0.U
        btb_is_ret(i) := false.B
        btb_is_call(i) := false.B
      }
      for (i <- 0 until rasEntries) { ras(i) := 0.U }
      ras_head := 0.U
      ras_depth := 0.U
    }
  }

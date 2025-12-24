package riscv_ooo

import hdl._

case class ROBEntry(
  valid: Bool,
  done: Bool,
  uop: UOp,
  wb_data: UInt
) extends Bundle[ROBEntry]

object ROBEntry:
  def apply(p: CoreParams): ROBEntry =
    ROBEntry(
      valid = Bool(),
      done = Bool(),
      uop = UOp(p),
      wb_data = UInt(p.xlenBits.W)
    )

case class ROBIO(
  dispatch_req: Vec[Valid[UOp]],
  dispatch_rob_idxs: Vec[UInt],
  wb_req: Vec[Valid[UOp]],
  wb_data: Vec[UInt],
  commit: Vec[Valid[UOp]],
  commit_data: Vec[UInt],
  full:  Bool,
  empty: Bool,
  valid_entries: UInt,
) extends Bundle[ROBIO]

class ROB(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(ROBIO(
    dispatch_req = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    dispatch_rob_idxs = Output(Vec.fill(p.coreWidth)(UInt(p.robIdxBits.W))),
    wb_req = Input(Vec.fill(p.issueWidth)(Valid(UOp(p)))),
    wb_data = Input(Vec.fill(p.issueWidth)(UInt(p.xlenBits.W))),
    commit = Output(Vec.fill(p.retireWidth)(Valid(UOp(p)))),
    commit_data = Output(Vec.fill(p.retireWidth)(UInt(p.xlenBits.W))),
    full = Output(Bool()),
    empty = Output(Bool()),
    valid_entries = Output(UInt(log2Ceil(p.robEntries + 1).W)),
  ))

  body {
    val numRows = p.robRows
    val entries = p.robEntries
    val coreWidth = p.coreWidth

    val rob_entries = Reg(Vec.fill(numRows)(Vec.fill(coreWidth)(ROBEntry(p))))

    val rob_head = RegInit(0.U(p.robIdxBits.W))
    val rob_tail = RegInit(0.U(p.robIdxBits.W))
    val maybe_full = RegInit(false.B)

    val full  = (rob_head === rob_tail) && maybe_full
    val empty = (rob_head === rob_tail) && !maybe_full

    io.full := full
    io.empty := empty

    val valid_entries = Wire(UInt(log2Ceil(entries + 1).W))
    valid_entries := Mux(rob_tail >= rob_head,
                            rob_tail - rob_head,
                            entries.U - (rob_head - rob_tail))

    io.valid_entries := valid_entries

    def robIdxToRow(idx: UInt): UInt = idx >> log2Ceil(coreWidth)
    def robIdxToBank(idx: UInt): UInt = idx(log2Ceil(coreWidth)-1, 0)
    def makeRobIdx(row: UInt, bank: UInt): UInt = Cat(Seq(row, bank))


    // -----------------------------------------------------------------------
    // Dispatch Logic
    // -----------------------------------------------------------------------
    val dispatch_fire = !io.full && io.dispatch_req.map(_.valid).reduce(_ || _)
    val dispatch_cnt  = io.dispatch_req.map(_.valid.asUInt).reduce(_ +& _)

    def idx_offset(ptr: UInt, off: Int): UInt =
      (ptr + off.U) % entries.U

    def bump_ptr(ptr: UInt, amount: UInt): Unit =
      ptr := (ptr + amount) % entries.U

    for (i <- 0 until coreWidth) {
      val req = io.dispatch_req(i)
      val rob_idx = idx_offset(rob_tail, i)
      val row  = robIdxToRow(rob_idx)
      val bank = robIdxToBank(rob_idx)

      io.dispatch_rob_idxs(i) := rob_idx
      when (dispatch_fire) {
        rob_entries(row)(bank).valid := req.valid
        rob_entries(row)(bank).done  := false.B
        rob_entries(row)(bank).uop   := req.bits
      }
    }

    when (dispatch_fire) {
      bump_ptr(rob_tail, dispatch_cnt)
    }

    for (i <- 0 until p.issueWidth) {
      val req = io.wb_req(i)
      val rob_idx = req.bits.rob_idx
      val row  = robIdxToRow(rob_idx)
      val bank = robIdxToBank(rob_idx)

      when (req.valid) {
        rob_entries(row)(bank).done := true.B
        rob_entries(row)(bank).wb_data := io.wb_data(i)
      }
    }

    val commit_mask = Wire(Vec.fill(p.retireWidth)(Bool()))
    for (i <- 0 until p.retireWidth) {
      val rob_idx = idx_offset(rob_head, i)
      val row  = robIdxToRow(rob_idx)
      val bank = robIdxToBank(rob_idx)
      val entry = rob_entries(row)(bank)
      val ready = entry.valid && entry.done

      if (i == 0) {
        commit_mask(i) := ready
      } else {
        commit_mask(i) := ready && commit_mask(i - 1)
      }

      io.commit(i).valid := commit_mask(i)
      io.commit(i).bits := entry.uop
      io.commit_data(i) := Mux(commit_mask(i), entry.wb_data, 0.U)
    }

    val commit_fire = commit_mask.reduce(_ || _)
    val commit_cnt  = commit_mask.map(_.asUInt).reduce(_ +& _)
    when (commit_fire) {
      for (i <- 0 until p.retireWidth) {
        when (commit_mask(i)) {
          val rob_idx = idx_offset(rob_head, i)
          val row  = robIdxToRow(rob_idx)
          val bank = robIdxToBank(rob_idx)
          rob_entries(row)(bank).valid := false.B
          rob_entries(row)(bank).done := false.B
        }
      }

      bump_ptr(rob_head, commit_cnt)
    }

    //////////////////////////////////////////////////////////////////////////

    when (reset.asBool) {
      rob_entries.foreach(row => row.foreach(entry => {
        entry.valid := false.B
        entry.done := false.B
      }))
    }

    dontTouch(io)
    dontTouch(rob_entries)
    dontTouch(rob_head)
    dontTouch(rob_tail)
  }

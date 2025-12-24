package riscv_ooo

import hdl._

case class ROBEntry(
  valid: Bool,
  uop: UOp
) extends Bundle[ROBEntry]

object ROBEntry:
  def apply(p: CoreParams): ROBEntry =
    ROBEntry(
      valid = Bool(),
      uop = UOp(p)
    )

case class ROBIO(
  dispatch_req: Vec[Valid[UOp]],
  dispatch_rob_idxs: Vec[UInt],
// wb_reqs: Vec[Valid[UInt]],
  commit: Vec[Valid[UOp]],
  full:  Bool,
  empty: Bool,
  valid_entries: UInt,
) extends Bundle[ROBIO]

class ROB(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(ROBIO(
    dispatch_req = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    dispatch_rob_idxs = Output(Vec.fill(p.coreWidth)(UInt(p.robIdxBits.W))),
// wb_reqs = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.robIdxBits.W)))),
    commit = Input(Vec.fill(p.retireWidth)(Valid(UOp(p)))),
    full = Output(Bool()),
    empty = Output(Bool()),
    valid_entries = Output(UInt(log2Ceil(p.robEntries + 1).W)),
  ))

  body {
    val numRows = p.robRows
    val entries = p.robEntries
    val coreWidth = p.coreWidth

    val rob_entries = Reg(Vec.fill(numRows)(Vec.fill(coreWidth)(ROBEntry(p))))

    val rob_head = RegInit(0.U(p.robRowIdxBits.W))
    val rob_tail = RegInit(0.U(p.robRowIdxBits.W))
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

    when (dispatch_fire) {
      for (i <- 0 until coreWidth) {
        val req = io.dispatch_req(i)
        val rob_idx = idx_offset(rob_tail, i)

        val row  = robIdxToRow(rob_idx)
        val bank = robIdxToBank(rob_idx)

        io.dispatch_rob_idxs(i) := rob_idx
        rob_entries(row)(bank).valid := req.valid
        rob_entries(row)(bank).uop   := req.bits
      }
      bump_ptr(rob_tail, dispatch_cnt)
    }

    // -----------------------------------------------------------------------
    // Commit Logic
    // -----------------------------------------------------------------------
    val commit_fire = io.commit.map(_.valid).reduce(_ || _)
    val commit_cnt  = io.commit.map(_.valid.asUInt).reduce(_ +& _)
    when (commit_fire) {

      for (i <- 0 until p.retireWidth) {
        val req = io.commit(i)
        val rob_idx = req.bits.rob_idx
        val row  = robIdxToRow(rob_idx)
        val bank = robIdxToBank(rob_idx)

        rob_entries(row)(bank).valid := false.B
      }

      bump_ptr(rob_head, dispatch_cnt)
    }

    //////////////////////////////////////////////////////////////////////////

    when (reset.asBool) {
      rob_entries.foreach(row => row.foreach(entry => entry.valid := false.B))
    }

    dontTouch(io)
    dontTouch(rob_entries)
    dontTouch(rob_head)
    dontTouch(rob_tail)
  }

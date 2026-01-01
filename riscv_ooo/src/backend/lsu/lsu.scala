package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.CoreConstants._

case class LDQEntry(
  valid: Bool,
  addr: Valid[UInt],               // Physical address (valid after AGU)
  executed: Bool,                  // Sent to D-cache
  succeeded: Bool,                 // D-cache response received

  next_stq_idx: UInt,              // All stores BEFORE this load (older stores)

  uop: UOp
) extends Bundle[LDQEntry]

object LDQEntry:
  def apply(p: CoreParams): LDQEntry =
    LDQEntry(
      valid = Bool(),
      addr = Valid(UInt(p.paddrBits.W)),

      executed = Bool(),
      succeeded = Bool(),

      next_stq_idx = UInt(log2Ceil(p.lsu.sqEntries + 1).W),

      uop = UOp(p),
    )

case class STQEntry(
  valid: Bool,
  addr: Valid[UInt],               // Physical address (valid after AGU)
  data: Valid[UInt],               // Store data (valid after data ready)

  committed: Bool,                 // ROB committed this store
  executed: Bool,                  // Sent request to D-cache
  succeeded: Bool,                 // D-cache acknowledged

  next_ldq_idx: UInt,              // All loads before this store (older loads)
  uop: UOp,
) extends Bundle[STQEntry]

object STQEntry:
  def apply(p: CoreParams): STQEntry =
    STQEntry(
      valid = Bool(),
      addr = Valid(UInt(p.paddrBits.W)),
      data = Valid(UInt(p.xlenBits.W)),

      committed = Bool(),
      executed = Bool(),
      succeeded = Bool(),

      next_ldq_idx = UInt(log2Ceil(p.lsu.lqEntries + 1).W),

      uop = UOp(p),
    )

case class LSUDispatchReq(
  uop: UOp,
  addr: UInt,
  data: UInt,
  size: HWEnum[MemWidth],
  signed: Bool,
) extends Bundle[LSUDispatchReq]

object LSUDispatchReq:
  def apply(p: CoreParams): LSUDispatchReq =
    LSUDispatchReq(
      uop = UOp(p),
      addr = UInt(p.paddrBits.W),
      data = UInt(p.xlenBits.W),
      size = HWEnum(MemWidth),
      signed = Bool()
    )

case class LSUResp(
  data: UInt,
  prd:  UInt,
  lrd:  UInt,
  rob_idx: UInt,
) extends Bundle[LSUResp]

object LSUResp:
  def apply(p: CoreParams): LSUResp =
    LSUResp(
      data = UInt(p.xlenBits.W),
      prd = UInt(p.pRegIdxBits.W),
      lrd = UInt(5.W),
      rob_idx = UInt(p.robIdxBits.W)
    )

case class LSUExeUpdate(
  is_load: Bool,
  is_store: Bool,
  ldq_idx: UInt,
  stq_idx: UInt,
  addr: UInt,
  data: UInt,
) extends Bundle[LSUExeUpdate]

object LSUExeUpdate:
  def apply(p: CoreParams): LSUExeUpdate =
    LSUExeUpdate(
      is_load = Bool(),
      is_store = Bool(),
      ldq_idx = UInt(p.ldqIdxBits.W),
      stq_idx = UInt(p.stqIdxBits.W),
      addr = UInt(p.paddrBits.W),
      data = UInt(p.xlenBits.W),
    )

case class LSUBundle(
  // Dispatch interface
  dispatch: Vec[Decoupled[UOp]],
  dispatch_ldq_idx: Vec[UInt],
  dispatch_stq_idx: Vec[UInt],

  // Execute update interface
  exe_update: Vec[Valid[LSUExeUpdate]],

  // Writeback interface
  ld_resp: Vec[Valid[LSUResp]],

  // Commit interface (from ROB)
  commit_store: Vec[Valid[UInt]],

  // Branch resolution interface
  br_resolve: BranchResolve,

  // Memory interface
  mem: MagicMemIf,
) extends Bundle[LSUBundle]

object LSUBundle:
  def apply(p: CoreParams): LSUBundle =
    LSUBundle(
      dispatch = Flipped(Vec.fill(p.coreWidth)(Decoupled(UOp(p)))),
      dispatch_ldq_idx = Output(Vec.fill(p.coreWidth)(UInt(p.ldqIdxBits.W))),
      dispatch_stq_idx = Output(Vec.fill(p.coreWidth)(UInt(p.stqIdxBits.W))),

      exe_update = Input(Vec.fill(p.intIssueWidth)(Valid(LSUExeUpdate(p)))),

      ld_resp = Output(Vec.fill(p.lsuIssueWidth)(Valid(LSUResp(p)))),

      commit_store = Input(Vec.fill(p.retireWidth)(Valid(UInt(p.robIdxBits.W)))),

      br_resolve = Input(BranchResolve(p)),

      mem = MagicMemIf(p),
    )

class LSU(p: CoreParams) extends Module:
  given Module = this

  val io = IO(LSUBundle(p))

  val numLdqEntries = p.lsu.lqEntries
  val numStqEntries = p.lsu.sqEntries
  val ldqIdxBits = p.ldqIdxBits
  val stqIdxBits = p.stqIdxBits

  body {
    val dcache = Module(new DCache(p))
    dcache.io.mem <> io.mem

    val ldq = Reg(Vec.fill(numLdqEntries)(LDQEntry(p)))
    val ldq_head = RegInit(0.U(ldqIdxBits.W))
    val ldq_tail = RegInit(0.U(ldqIdxBits.W))

    def ldq_ptr_match = ldq_head(ldqIdxBits-2, 0) === ldq_tail(ldqIdxBits-2, 0)
    def ldq_empty = ldq_ptr_match && (ldq_head(ldqIdxBits-1) === ldq_tail(ldqIdxBits-1))
    def ldq_full = ldq_ptr_match && (ldq_head(ldqIdxBits-1) =/= ldq_tail(ldqIdxBits-1))

    def wrap_incr_ldq(ptr: UInt, amt: UInt): UInt = {
      val incremented = ptr + amt
      Mux(incremented >= numLdqEntries.U,
          incremented - numLdqEntries.U,
          incremented)
    }

    val stq = Reg(Vec.fill(numStqEntries)(STQEntry(p)))
    val stq_head = RegInit(0.U(stqIdxBits.W))      // Oldest store
    val stq_commit_head = RegInit(0.U(stqIdxBits.W)) // Oldest uncommitted
    val stq_tail = RegInit(0.U(stqIdxBits.W))      // Youngest store + 1

    def stq_ptr_match = stq_head(stqIdxBits-2, 0) === stq_tail(stqIdxBits-2, 0)
    def stq_empty = stq_ptr_match && (stq_head(stqIdxBits-1) === stq_tail(stqIdxBits-1))
    def stq_full = stq_ptr_match && (stq_head(stqIdxBits-1) =/= stq_tail(stqIdxBits-1))

    def wrap_incr_stq(ptr: UInt, amt: UInt): UInt = {
      val incremented = ptr + amt
      Mux(incremented >= numStqEntries.U,
          incremented - numStqEntries.U,
          incremented)
    }

    // ------------------------------------------------------------------------
    // Dispatch: Allocate LDQ/STQ entries
    // ------------------------------------------------------------------------
    val num_loads_dispatching = Wire(UInt(log2Ceil(p.coreWidth + 1).W))
    val num_stores_dispatching = Wire(UInt(log2Ceil(p.coreWidth + 1).W))

    num_loads_dispatching  := io.dispatch.map(d => (d.valid && d.bits.ctrl.is_load ).asUInt).reduce(_ + _)
    num_stores_dispatching := io.dispatch.map(d => (d.valid && d.bits.ctrl.is_store).asUInt).reduce(_ + _)

    val ldq_slots_available = Mux(ldq_full, 0.U,
                              Mux(ldq_tail >= ldq_head,
                                  numLdqEntries.U - (ldq_tail - ldq_head),
                                  ldq_head - ldq_tail)).asWire

    val stq_slots_available = Mux(stq_full, 0.U,
                              Mux(stq_tail >= stq_head,
                                  numStqEntries.U - (stq_tail - stq_head),
                                  stq_head - stq_tail)).asWire

    // Pessimistic backpressure, but ok to keep things simple
    val can_allocate_loads = num_loads_dispatching <= ldq_slots_available
    val can_allocate_stores = num_stores_dispatching <= stq_slots_available
    val can_dispatch = can_allocate_loads && can_allocate_stores

    val ldq_alloc_idx = Wire(Vec.fill(p.coreWidth)(UInt(p.ldqIdxBits.W)))
    val stq_alloc_idx = Wire(Vec.fill(p.coreWidth)(UInt(p.stqIdxBits.W)))

    io.dispatch_ldq_idx.foreach(_ := DontCare)
    io.dispatch_stq_idx.foreach(_ := DontCare)

    var load_allocated = 0.U(log2Ceil(p.coreWidth + 1).W)
    var store_allocated = 0.U(log2Ceil(p.coreWidth + 1).W)

    for (w <- 0 until p.coreWidth) {
      val is_load  = io.dispatch(w).bits.ctrl.is_load
      val is_store = io.dispatch(w).bits.ctrl.is_store

      ldq_alloc_idx(w) := wrap_incr_ldq(ldq_tail, load_allocated)
      stq_alloc_idx(w) := wrap_incr_stq(stq_tail, store_allocated)

      io.dispatch(w).ready := can_dispatch

      when (io.dispatch(w).fire && is_load) {
        val lidx = ldq_alloc_idx(w)
        val uop = io.dispatch(w).bits

        ldq(lidx).valid := true.B
        ldq(lidx).addr.valid := false.B
        ldq(lidx).addr.bits := DontCare
        ldq(lidx).executed := false.B
        ldq(lidx).succeeded := false.B
        ldq(lidx).next_stq_idx := wrap_incr_stq(stq_tail, store_allocated)
        ldq(lidx).uop := uop

        io.dispatch_ldq_idx(w) := lidx
      }

      when (io.dispatch(w).fire && is_store) {
        val sidx = stq_alloc_idx(w)
        val uop = io.dispatch(w).bits

        stq(sidx).valid := true.B
        stq(sidx).addr.valid := false.B
        stq(sidx).addr.bits := DontCare
        stq(sidx).data.valid := false.B
        stq(sidx).data.bits := DontCare
        stq(sidx).committed := false.B
        stq(sidx).executed := false.B
        stq(sidx).succeeded := false.B
        stq(sidx).next_ldq_idx := wrap_incr_ldq(ldq_tail, load_allocated)
        stq(sidx).uop := uop

        io.dispatch_stq_idx(w) := sidx
      }

      load_allocated = load_allocated + (io.dispatch(w).fire && is_load).asUInt
      store_allocated = store_allocated + (io.dispatch(w).fire && is_store).asUInt
    }

    when (can_dispatch) {
      ldq_tail := wrap_incr_ldq(ldq_tail, num_loads_dispatching)
      stq_tail := wrap_incr_stq(stq_tail, num_stores_dispatching)
    }

    // ------------------------------------------------------------------------
    // Execute updates
    // ------------------------------------------------------------------------

    def brKill(br_mask: UInt): Bool =
      io.br_resolve.valid &&
      io.br_resolve.mispredict &&
      (br_mask & io.br_resolve.tag) =/= 0.U

    for (w <- 0 until p.intIssueWidth) {
      when (io.exe_update(w).valid) {
        val upd = io.exe_update(w).bits

        when (upd.is_load) {
          val idx = upd.ldq_idx
          when (ldq(idx).valid && !brKill(ldq(idx).uop.br_mask)) {
            ldq(idx).addr.valid := true.B
            ldq(idx).addr.bits := upd.addr
          }
        }

        when (upd.is_store) {
          val idx = upd.stq_idx
          when (stq(idx).valid && !brKill(stq(idx).uop.br_mask)) {
            stq(idx).addr.valid := true.B
            stq(idx).addr.bits := upd.addr
            stq(idx).data.valid := true.B
            stq(idx).data.bits := upd.data
          }
        }
      }
    }

  }

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

case class LSUCommitBundle(
  is_load: Bool,
  ldq_idx: UInt,
  stq_idx: UInt,
) extends Bundle[LSUCommitBundle]

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
  commit: Vec[Valid[LSUCommitBundle]],

  // Branch resolution interface
  br_resolve: BranchResolve,

  // DCache interface
  dcache: LSUDCacheIO,

  // Memory interface (for now, for placeholder dcache)
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

      commit = Input(Vec.fill(p.retireWidth)(Valid(LSUCommitBundle(
        is_load = Bool(),
        ldq_idx = UInt(p.ldqIdxBits.W),
        stq_idx = UInt(p.stqIdxBits.W),
      )))),

      br_resolve = Input(BranchResolve(p)),

      dcache = LSUDCacheIO(p),

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
    // DCache will be connected externally or implemented separately
    // For now, interface signals defined in LSUDCacheIO

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

    // ------------------------------------------------------------------------
    // Branch misprediction recovery
    // ------------------------------------------------------------------------
    def updateBrMask(br_mask: UInt): UInt =
      Mux(io.br_resolve.valid && !io.br_resolve.mispredict,
        br_mask & ~io.br_resolve.tag,
        br_mask)

    // Update and kill LDQ entries on branch misprediction
    for (i <- 0 until numLdqEntries) {
      when (ldq(i).valid) {
        val new_br_mask = updateBrMask(ldq(i).uop.br_mask)
        ldq(i).uop.br_mask := new_br_mask

        when (brKill(ldq(i).uop.br_mask)) {
          ldq(i).valid := false.B
          ldq(i).addr.valid := false.B
        }
      }
    }

    // Update and kill STQ entries on branch misprediction
    for (i <- 0 until numStqEntries) {
      when (stq(i).valid) {
        val new_br_mask = updateBrMask(stq(i).uop.br_mask)
        stq(i).uop.br_mask := new_br_mask

        when (brKill(stq(i).uop.br_mask)) {
          stq(i).valid := false.B
          stq(i).addr.valid := false.B
          stq(i).data.valid := false.B
        }
      }
    }

    // Reset tail pointers on misprediction (like BOOM)
    when (io.br_resolve.valid && io.br_resolve.mispredict) {
      ldq_tail := io.br_resolve.ldq_idx
      stq_tail := io.br_resolve.stq_idx
    }

    // ------------------------------------------------------------------------
    // Memory Disambiguation: Check for address conflicts with older stores
    // ------------------------------------------------------------------------
    def addr_match(ld_addr: UInt, ld_size: HWEnum[MemWidth],
                   st_addr: UInt, st_size: HWEnum[MemWidth]): Bool = {
      val ld_size_bytes = Mux(ld_size === MemWidth.B.EN, 1.U,
                          Mux(ld_size === MemWidth.H.EN, 2.U,
                          Mux(ld_size === MemWidth.W.EN, 4.U, 8.U)))
      val st_size_bytes = Mux(st_size === MemWidth.B.EN, 1.U,
                          Mux(st_size === MemWidth.H.EN, 2.U,
                          Mux(st_size === MemWidth.W.EN, 4.U, 8.U)))

      val ld_end = ld_addr + ld_size_bytes
      val st_end = st_addr + st_size_bytes

      val no_overlap = (ld_end <= st_addr) || (st_end <= ld_addr)
      !no_overlap
    }

    def stores_between(start_idx: UInt, end_idx: UInt): Vec[Bool] = {
      val in_range = Wire(Vec.fill(numStqEntries)(Bool()))
      for (i <- 0 until numStqEntries) {
        val i_u = i.U
        when (start_idx <= end_idx) {
          in_range(i) := (i_u >= start_idx) && (i_u < end_idx)
        } .otherwise {
          in_range(i) := (i_u >= start_idx) || (i_u < end_idx)
        }
      }
      in_range
    }

    val ld_has_conflict = Wire(Vec.fill(numLdqEntries)(Bool()))
    for (i <- 0 until numLdqEntries) {
      val older_stores = stores_between(stq_head, ldq(i).next_stq_idx)

      var has_unknown_addr = false.B
      var has_addr_match = false.B

      for (s <- 0 until numStqEntries) {
        when (older_stores(s) && stq(s).valid) {
          when (!stq(s).addr.valid) {
            has_unknown_addr = true.B
          } .elsewhen (ldq(i).addr.valid &&
                       addr_match(ldq(i).addr.bits, ldq(i).uop.ctrl.mem_width,
                                  stq(s).addr.bits, stq(s).uop.ctrl.mem_width)) {
            has_addr_match = true.B
          }
        }
      }

      ld_has_conflict(i) := has_unknown_addr || has_addr_match
    }

    // ------------------------------------------------------------------------
    // Select loads to fire to DCache
    // ------------------------------------------------------------------------
    val lsuWidth = p.lsuIssueWidth

    val can_fire_load = Wire(Vec.fill(numLdqEntries)(Bool()))
    for (i <- 0 until numLdqEntries) {
      can_fire_load(i) :=
        ldq(i).valid &&
        ldq(i).addr.valid &&
        !ldq(i).executed &&
        !brKill(ldq(i).uop.br_mask) &&
        !ld_has_conflict(i)
    }

    val can_fire_store = Wire(Vec.fill(numStqEntries)(Bool()))
    for (i <- 0 until numStqEntries) {
      can_fire_store(i) :=
        stq(i).valid &&
        stq(i).addr.valid &&
        stq(i).data.valid &&
        stq(i).committed &&
        !stq(i).executed &&
        !brKill(stq(i).uop.br_mask)
    }

    // Simple priority selection
    val fire_load_idx = Wire(Vec.fill(lsuWidth)(Valid(UInt(ldqIdxBits.W))))
    val fire_store_idx = Wire(Vec.fill(lsuWidth)(Valid(UInt(stqIdxBits.W))))

    for (w <- 0 until lsuWidth) {
      fire_load_idx(w).valid := false.B
      fire_load_idx(w).bits := DontCare
      fire_store_idx(w).valid := false.B
      fire_store_idx(w).bits := DontCare
    }

    // Find first ready load for each port (simple priority encoder)
    var load_found = 0.U(log2Ceil(lsuWidth + 1).W)
    for (i <- 0 until numLdqEntries) {
      val fire = can_fire_load(i) && load_found < lsuWidth.U
      when (fire) {
        fire_load_idx(load_found).valid := true.B
        fire_load_idx(load_found).bits := i.U
      }
      load_found = Mux(fire, load_found + 1.U, load_found)
    }

    // Find first ready store for remaining ports
    var store_found = 0.U(log2Ceil(lsuWidth + 1).W)
    for (i <- 0 until numStqEntries) {
      val fire = can_fire_store(i) && (load_found + store_found) < lsuWidth.U
      when (fire) {
        fire_store_idx(store_found).valid := true.B
        fire_store_idx(store_found).bits := i.U
      }
      store_found = Mux(fire, store_found + 1.U, store_found)
    }

    for (w <- 0 until lsuWidth) {
      io.dcache.req(w).valid := false.B
      io.dcache.req(w).bits := DontCare
      io.dcache.s1_kill(w) := false.B
    }

    for (w <- 0 until lsuWidth) {
      when (fire_load_idx(w).valid) {
        val idx = fire_load_idx(w).bits
        val ldq_e = ldq(idx)

        io.dcache.req(w).valid := true.B
        io.dcache.req(w).bits.addr := ldq_e.addr.bits
        io.dcache.req(w).bits.data := DontCare
        io.dcache.req(w).bits.size := ldq_e.uop.ctrl.mem_width
        io.dcache.req(w).bits.signed := ldq_e.uop.ctrl.mem_signed
        io.dcache.req(w).bits.is_load := true.B
        io.dcache.req(w).bits.is_store := false.B
        io.dcache.req(w).bits.ldq_idx := idx
        io.dcache.req(w).bits.stq_idx := DontCare
        io.dcache.req(w).bits.rob_idx := ldq_e.uop.rob_idx
        io.dcache.req(w).bits.br_mask := ldq_e.uop.br_mask

        when (io.dcache.req(w).fire) {
          ldq(idx).executed := true.B
        }
      }
    }

    var store_port = 0.U(log2Ceil(lsuWidth + 1).W)
    for (w <- 0 until lsuWidth) {
      when (!fire_load_idx(w).valid && fire_store_idx(w).valid) {
        val idx = fire_store_idx(w).bits
        val stq_e = stq(idx)

        io.dcache.req(w).valid := true.B
        io.dcache.req(w).bits.addr := stq_e.addr.bits
        io.dcache.req(w).bits.data := stq_e.data.bits
        io.dcache.req(w).bits.size := stq_e.uop.ctrl.mem_width
        io.dcache.req(w).bits.signed := DontCare
        io.dcache.req(w).bits.is_load := false.B
        io.dcache.req(w).bits.is_store := true.B
        io.dcache.req(w).bits.ldq_idx := DontCare
        io.dcache.req(w).bits.stq_idx := idx
        io.dcache.req(w).bits.rob_idx := stq_e.uop.rob_idx
        io.dcache.req(w).bits.br_mask := stq_e.uop.br_mask

        when (io.dcache.req(w).fire) {
          stq(idx).executed := true.B
        }
      }
    }

    // ------------------------------------------------------------------------
    // DCache response handling
    // ------------------------------------------------------------------------

    // Handle fast responses (cache hits)
    for (w <- 0 until lsuWidth) {
      when (io.dcache.resp(w).valid) {
        val resp = io.dcache.resp(w).bits
        val idx = resp.ldq_idx

        when (ldq(idx).valid) {
          ldq(idx).succeeded := true.B
        }
      }
    }

    // Handle nacks - need to retry
    for (w <- 0 until lsuWidth) {
      when (io.dcache.nack(w).valid) {
        val nack_req = io.dcache.nack(w).bits
        when (nack_req.is_load) {
          val idx = nack_req.ldq_idx
          ldq(idx).executed := false.B
        } .elsewhen (nack_req.is_store) {
          val idx = nack_req.stq_idx
          stq(idx).executed := false.B
        }
      }
    }

    // Handle store acknowledgments
    for (w <- 0 until lsuWidth) {
      when (io.dcache.store_ack(w).valid) {
        val idx = io.dcache.store_ack(w).bits.stq_idx
        when (stq(idx).valid) {
          stq(idx).succeeded := true.B
        }
      }
    }

    // Handle long-latency MSHR responses (cache misses)
    io.dcache.ll_resp.ready := true.B
    when (io.dcache.ll_resp.valid) {
      val resp = io.dcache.ll_resp.bits
      val idx = resp.ldq_idx

      when (ldq(idx).valid) {
        ldq(idx).succeeded := true.B
      }
    }

    // ------------------------------------------------------------------------
    // Load writeback
    // ------------------------------------------------------------------------

    io.ld_resp.foreach(r => {
      r.valid := false.B
      r.bits := DontCare
    })

    var resp_idx = 0.U
    for (i <- 0 until numLdqEntries) {
      when (ldq(i).valid && ldq(i).succeeded && resp_idx < lsuWidth.U) {
        // Find data from either fast resp or ll_resp
        val resp_data = Wire(UInt(p.xlenBits.W))
        resp_data := DontCare

        // Check fast responses
        for (w <- 0 until lsuWidth) {
          when (io.dcache.resp(w).valid && io.dcache.resp(w).bits.ldq_idx === i.U) {
            resp_data := io.dcache.resp(w).bits.data
          }
        }

        // Check long-latency response
        when (io.dcache.ll_resp.valid && io.dcache.ll_resp.bits.ldq_idx === i.U) {
          resp_data := io.dcache.ll_resp.bits.data
        }

        io.ld_resp(resp_idx).valid := true.B
        io.ld_resp(resp_idx).bits.data := resp_data
        io.ld_resp(resp_idx).bits.prd := ldq(i).uop.prd
        io.ld_resp(resp_idx).bits.lrd := ldq(i).uop.lrd
        io.ld_resp(resp_idx).bits.rob_idx := ldq(i).uop.rob_idx

        resp_idx = resp_idx + 1.U
      }
    }

    // ------------------------------------------------------------------------
    // Free entries
    // ------------------------------------------------------------------------

    for (w <- 0 until p.retireWidth) {
      val commit = io.commit(w)
      when (commit.valid && !commit.bits.is_load) {
        stq(commit.bits.stq_idx).committed := true.B
      }
    }

    var num_ldq_freed = 0.U
    for (w <- 0 until p.retireWidth) {
      val idx = wrap_incr_ldq(ldq_head, num_ldq_freed)
      val free = ldq(idx).valid && ldq(idx).succeeded && num_ldq_freed < p.retireWidth.U
      when (free) {
        ldq(idx).valid := false.B
      }
      num_ldq_freed = Mux(free, num_ldq_freed + 1.U, num_ldq_freed)
    }
    ldq_head := wrap_incr_ldq(ldq_head, num_ldq_freed)

    var num_stq_freed = 0.U
    for (w <- 0 until p.retireWidth) {
      val idx = wrap_incr_stq(stq_head, num_stq_freed)
      val free = stq(idx).valid && stq(idx).succeeded && stq(idx).committed && num_stq_freed < p.retireWidth.U
      when (free) {
        stq(idx).valid := false.B
      }
      num_stq_freed = Mux(free, num_stq_freed + 1.U, num_stq_freed)
    }
    stq_head := wrap_incr_stq(stq_head, num_stq_freed)

    io.mem <> DontCare
  }

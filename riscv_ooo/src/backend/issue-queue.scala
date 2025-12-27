package riscv_ooo

import hdl._

case class IssueQueueEntry(
  valid: Bool,
  uop: UOp,
) extends Bundle[IssueQueueEntry]

object IssueQueueEntry:
  def apply(p: CoreParams): IssueQueueEntry =
    IssueQueueEntry(
      valid = Bool(),
      uop = UOp(p),
    )

case class IssueQueueIO(
  dis_uops: Vec[Valid[UOp]],
  dis_ready: Bool,
  wakeup_idx: Vec[Valid[UInt]],
  issue_uops: Vec[Valid[UOp]],
  mispredict: Valid[OneHot],
  resolve_tag: Valid[OneHot]
) extends Bundle[IssueQueueIO]

class IssueQueue(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val numEntries = p.isq.numEntries
  val issueWidth = p.issueWidth

  val io = IO(IssueQueueIO(
    dis_uops    = Input(Vec.fill(p.coreWidth)(Valid(UOp(p)))),
    dis_ready   = Output(Bool()),
    wakeup_idx  = Input(Vec.fill(p.coreWidth)(Valid(UInt(p.pRegIdxBits.W)))),
    issue_uops  = Output(Vec.fill(issueWidth)(Valid(UOp(p)))),
    mispredict  = Input(Valid(OneHot(p.branchTagBits.W))),
    resolve_tag = Input(Valid(OneHot(p.branchTagBits.W)))
  ))

  body {
    val entries = Reg(Vec.fill(numEntries)(IssueQueueEntry(p)))

    val valid_mask = entries.map(_.valid)
    val free_mask  = valid_mask.map(!_)
    val num_free   = free_mask.map(_.asUInt).reduce(_ +& _)

    // TODO: Fix w/ actual backpressure
    io.dis_ready := num_free >= p.coreWidth.U

    // -------------------------------------------------------------------------
    // Wakeup Logic
    // -------------------------------------------------------------------------
    for (e <- entries) {
      for (wk <- io.wakeup_idx) {
        when (wk.valid && e.valid) {
          when (e.uop.prs1_busy && e.uop.prs1 === wk.bits) {
            e.uop.prs1_busy := false.B
          }
          when (e.uop.prs2_busy && e.uop.prs2 === wk.bits) {
            e.uop.prs2_busy := false.B
          }
        }
      }
    }

    // -------------------------------------------------------------------------
    // Kill Logic (on misprediction)
    // -------------------------------------------------------------------------
    when (io.mispredict.valid) {
      for (e <- entries) {
        when (e.valid && (e.uop.branch_mask & io.mispredict.bits.asUInt) =/= 0.U) {
          e.valid := false.B
        }
      }
    }

    // -------------------------------------------------------------------------
    // Clear resolved branch tag from masks
    // -------------------------------------------------------------------------
    when (io.resolve_tag.valid && !io.mispredict.valid) {
      for (e <- entries) {
        when (e.valid) {
          e.uop.branch_mask := e.uop.branch_mask & ~io.resolve_tag.bits.asUInt
        }
      }
    }

    // -------------------------------------------------------------------------
    // Issue Select Logic (Age-based)
    // -------------------------------------------------------------------------

    //  true: entry i is older than entry j
    val age_matrix = Reg(Vec.fill(numEntries)(Vec.fill(numEntries)(Bool())))

    val ready_mask = entries.map(e => {
      e.valid &&
      (!e.uop.prs1_busy || !e.uop.lrs1_val) &&
      (!e.uop.prs2_busy || !e.uop.lrs2_val)
    }).asWire
    dontTouch(ready_mask)

    val oldest_mask = Wire(Vec.fill(numEntries)(Bool()))
    for (i <- 0 until numEntries) {
      // entries(i): oldest if age_matrix(i)(j) = true for all other valid j
      val dominated = (0 until numEntries).map { j =>
        if (i == j) true.B
        else !valid_mask(j) || age_matrix(i)(j)
      }
      oldest_mask(i) := dominated.reduce(_ && _)
    }

    val issue_grants = Wire(Vec.fill(issueWidth)(UInt(numEntries.W)))
    var remaining_ready = Cat(ready_mask.reverse)
    var remaining_oldest = Cat(oldest_mask.reverse)

    var cfi_issued = WireInit(false.B)
    val is_cfi_mask = entries.map(e => e.valid && e.uop.ctrl.is_cfi)

    for (i <- 0 until issueWidth) {
      val candidates = Wire(UInt(numEntries.W))
      val select_from = Wire(UInt(numEntries.W))
      dontTouch(candidates)
      dontTouch(select_from)

      val cfi_mask_bits = Cat(is_cfi_mask.reverse)
      val non_cfi_ready = remaining_ready & ~cfi_mask_bits
      val cfi_ready = remaining_ready & cfi_mask_bits

      val effective_ready = Mux(cfi_issued, non_cfi_ready, remaining_ready)

      candidates := effective_ready & remaining_oldest
      select_from := Mux(candidates.orR, candidates, effective_ready)

      val grant = PriorityEncoderOH(select_from)
      issue_grants(i) := grant.asUInt

      val grant_idx = PriorityEncoder(grant)
      remaining_ready = remaining_ready & ~grant.asUInt

      val issued_is_cfi = (grant.asUInt & cfi_mask_bits).orR
      cfi_issued = cfi_issued || issued_is_cfi

      val new_oldest = (0 until numEntries).map { j =>
        val dominated = (0 until numEntries).map { k =>
          if (j == k) true.B
          else !remaining_ready(k).asBool || age_matrix(j)(k)
        }
        dominated.reduce(_ && _)
      }
      remaining_oldest = Cat(new_oldest.reverse)
    }

    for (i <- 0 until issueWidth) {
      val grant_oh = issue_grants(i).asOH
      io.issue_uops(i).valid := issue_grants(i).orR
      io.issue_uops(i).bits  := MuxOneHot(grant_oh, entries.map(_.uop).toSeq)
    }

    for (i <- 0 until numEntries) {
      val issued = issue_grants.map(g => g(i).asBool).reduce(_ || _)
      when (issued) {
        entries(i).valid := false.B
      }
    }

    // -------------------------------------------------------------------------
    // Dispatch Allocation Logic
    // -------------------------------------------------------------------------
    val free_slots = Wire(Vec.fill(p.coreWidth)(UInt(log2Ceil(numEntries + 1).W)))
    var remaining_free = Cat(free_mask.reverse)

    for (i <- 0 until p.coreWidth) {
      val slot_oh = PriorityEncoderOH(remaining_free)
      free_slots(i) := PriorityEncoder(slot_oh)
      remaining_free = remaining_free & ~slot_oh.asUInt
    }

    when (io.dis_ready) {
      for (i <- 0 until p.coreWidth) {
        when (io.dis_uops(i).valid) {
          val slot = free_slots(i)
          entries(slot).valid := true.B
          entries(slot).uop := io.dis_uops(i).bits

          for (j <- 0 until numEntries) {
            age_matrix(slot)(j) := false.B
            when (entries(j).valid) {
              age_matrix(j)(slot) := true.B
            }
          }
        }
      }
    }

    // -------------------------------------------------------------------------
    // Reset
    // -------------------------------------------------------------------------
    when (reset.asBool) {
      entries.foreach(e => e.valid := false.B)
      age_matrix.foreach(row => row.foreach(_ := false.B))
    }

    dontTouch(io)
    dontTouch(entries)
    dontTouch(age_matrix)
    dontTouch(issue_grants)
    dontTouch(oldest_mask)
  }

package riscv_ooo

import hdl._

case class BranchResolve(
  valid: Bool,
  tag: OneHot,
  mispredict: Bool,
) extends Bundle[BranchResolve]

object BranchResolve:
  def apply(p: CoreParams): BranchResolve =
    BranchResolve(
      valid = Bool(),
      tag = OneHot(p.branchTagBits.W),
      mispredict = Bool(),
    )

case class BranchTagManagerIO(
  req_valid: Vec[Bool],
  req_cfi: Vec[Bool],
  count: UInt,

  resp_mask: Vec[UInt],
  resp_tag:  Vec[OneHot],

  resolve_tag: BranchResolve,
) extends Bundle[BranchTagManagerIO]

class BranchTagManager(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val coreWidth = p.coreWidth
  val branchTagBits = p.branchTagBits

  val io = IO(BranchTagManagerIO(
    req_valid = Input(Vec.fill(coreWidth)(Bool())),
    req_cfi   = Input(Vec.fill(coreWidth)(Bool())),
    count     = Output(UInt(log2Ceil(branchTagBits + 1).W)),

    resp_mask = Output(Vec.fill(coreWidth)(UInt  (branchTagBits.W))),
    resp_tag  = Output(Vec.fill(coreWidth)(OneHot(branchTagBits.W))),

    resolve_tag = Input(BranchResolve(p)),
  ))

  body {

    val free_tags = RegInit(Lit(UInt(branchTagBits.W))((1 << branchTagBits) - 1))
    val cur_mask  = RegInit(Lit(UInt(branchTagBits.W))(0))
    val mask_snapshots = Reg(Vec.fill(branchTagBits)(Valid(UInt(branchTagBits.W))))

    io.count := PopCount(free_tags)
    io.resp_mask := DontCare
    io.resp_tag  := DontCare

    var next_free_tags = free_tags
    var next_cur_mask = cur_mask

    val next_freetags_base = Wire(Vec.fill(coreWidth)(UInt(branchTagBits.W)))
    val next_freetags_alloc= Wire(Vec.fill(coreWidth)(UInt(branchTagBits.W)))

    val next_mask_base = Wire(Vec.fill(coreWidth)(UInt(branchTagBits.W)))
    val next_mask_alloc = Wire(Vec.fill(coreWidth)(UInt(branchTagBits.W)))

    for (i <- 0 until coreWidth) {
      if i == 0 then
        next_freetags_base(i) := free_tags
        next_freetags_alloc(i) := free_tags

        next_mask_base(i) := cur_mask
        next_mask_alloc(i) := cur_mask
      else
        next_freetags_base(i) := next_freetags_alloc(i-1)
        next_freetags_alloc(i) := next_freetags_alloc(i-1)

        next_mask_base(i) := next_mask_alloc(i-1)
        next_mask_alloc(i) := next_mask_alloc(i-1)

      when (io.req_valid(i) && io.req_cfi(i)) {
        val tag_idx = PriorityEncoder(Reverse(free_tags))
        val tag_oh  = UIntToOH(tag_idx)

        next_mask_alloc(i) = next_mask_base(i) | tag_oh
        next_freetags_alloc(i) = next_freetags_base(i) & ~tag_oh

        mask_snapshots(tag_idx).valid := true.B
        mask_snapshots(tag_idx).bits  := next_mask_alloc(i)
        io.resp_mask(i) := next_mask_alloc(i)
        io.resp_tag(i)  := tag_oh
      }
    }

    when (io.resolve_tag.valid) {
      val tag_idx = PriorityEncoder(Reverse(io.resolve_tag.tag))
      mask_snapshots(tag_idx).valid := false.B

      val tags_to_return = Wire(Vec.fill(branchTagBits)(UInt(branchTagBits.W)))
      for (i <- 0 until branchTagBits) {
        val mask_hit = mask_snapshots(i).valid && ((mask_snapshots(i).bits & io.resolve_tag.tag.asUInt) =/= 0.U)
        when (mask_hit) {
          mask_snapshots(i).bits := mask_snapshots(i).bits & ~io.resolve_tag.tag
          when (io.resolve_tag.mispredict) {
            mask_snapshots(i).valid := false.B
            tags_to_return(i) := 1.U << i
          } .otherwise {
            tags_to_return(i) := 0.U
          }
        }
      }

      when (io.resolve_tag.mispredict) {
        Assert(!io.req_valid.reduce(_ || _),
          "BranchTagManager: Uops in the decode stage should be killed on a branch misprediction")

        free_tags := free_tags | tags_to_return.reduce(_ | _)
        cur_mask := mask_snapshots(tag_idx).bits & ~io.resolve_tag.tag.asUInt
      } .otherwise {
        free_tags := next_freetags_alloc(coreWidth-1) |  io.resolve_tag.tag.asUInt
        cur_mask  :=     next_mask_alloc(coreWidth-1) & ~io.resolve_tag.tag.asUInt
      }
    } .otherwise {
      free_tags := next_freetags_alloc(coreWidth-1)
      cur_mask  := next_mask_alloc(coreWidth-1)
    }

    when (reset.asBool) {
      mask_snapshots.map(_.valid).foreach(_ := false.B)
    }

    dontTouch(io)
  }

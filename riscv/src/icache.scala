package riscv

import hdl._

case class TagEntry(valid: Bool, tag: UInt) extends Bundle[TagEntry]
object TagEntry:
  def apply(tagBits: Int): TagEntry =
    TagEntry(valid = Bool(), tag = UInt(tagBits.W))


case class ICacheBundle(
  core: ICacheIf,
  mem: MagicMemIf
) extends Bundle[ICacheBundle]

object ICacheBundle:
  def apply(p: CoreParams): ICacheBundle =
    ICacheBundle(
      core = Flipped(ICacheIf(p)),
      mem  = MagicMemIf(p)
    )

// TODO: proper banking of data arrays...
class ICache(
  p: CoreParams,
) extends Module:
  given Module = this
  val io = IO(ICacheBundle(p))

  val ic = p.ic

  body {
    dontTouch(io)

    val nSets = ic.nSets
    val nWays = ic.nWays
    assert((nSets & (nSets-1)) == 0)
    assert((nWays & (nWays-1)) == 0)

    val lineBytes     = ic.cacheLineBytes
    val wordsPerLine  = lineBytes / 4

    val lineOffBits = log2Ceil(lineBytes)
    val setIdxBits  = log2Ceil(nSets)
    val tagBits = p.pcBits - (setIdxBits + lineOffBits)

    def VecTagEntry(entries: Int): Vec[TagEntry] =
      Vec.fill(entries)(TagEntry(tagBits))

    def VecDataEntry(entries: Int): Vec[UInt] =
      Vec.fill(entries)(UInt((8*ic.cacheLineBytes).W))

    val tag_array = SRAM(VecTagEntry(nWays), nSets)
                        (reads = 1, writes = 1, readwrites = 0, masked = true)

    val data_array = SRAM(VecDataEntry(nWays), nSets)
                         (reads = 1, writes = 1, readwrites = 0, masked = true)

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    def setIdx(addr: UInt): UInt =
      addr(lineOffBits + setIdxBits - 1, lineOffBits)

    def tagOf(addr: UInt): UInt =
      addr(p.pcBits-1, lineOffBits + setIdxBits)

    def clOffset(addr: UInt): UInt =
      addr(lineOffBits-1, log2Ceil(p.instBytes))

    io.mem.req.valid := false.B
    io.mem.req.bits.addr := DontCare

    val miss_busy     = RegInit(false.B)
    val miss_set      = Reg(UInt(setIdxBits.W))
    val miss_tag      = Reg(UInt(tagBits.W))
    val miss_victim   = Reg(UInt(log2Ceil(nWays).W))
    val miss_req_addr = Reg(UInt(p.pcBits.W))

    // -----------------------------------------------------------------------
    // Stage 0
    // - Perform tag lookup
    // -----------------------------------------------------------------------
    val s0_valid = io.core.s0_vaddr.valid && !miss_busy
    val s0_vaddr = io.core.s0_vaddr.bits

    val s0_set = setIdx(s0_vaddr)

    when (s0_valid) {
      tag_array.readPorts(0).read(s0_set)
    }

    // -----------------------------------------------------------------------
    // Stage 1
    // - Match tags and check hit/miss
    // -----------------------------------------------------------------------
    val s1_vaddr = RegNext(s0_vaddr)
    val s1_valid = RegInit(false.B)
    s1_valid := s0_valid

    val s1_set   = setIdx(s1_vaddr)
    val s1_paddr = io.core.s1_paddr.bits
    val s1_paddr_tag = tagOf(s1_paddr)
    val s1_tags = tag_array.readPorts(0).data

    val s1_tag_hit_vec = s1_tags.map(entry => {
      s1_valid &&
      entry.valid &&
      (entry.tag === s1_paddr_tag) &&
      io.core.s1_paddr.valid
    })

    val s1_tag_hit = s1_tag_hit_vec.reduce(_ || _)
    val s1_tag_hit_oh = Cat(s1_tag_hit_vec.reverse).asOH
    val s1_tag_hit_cnt = PopCount(s1_tag_hit_oh)
    Assert(s1_tag_hit_cnt <= 1.U, "Multiple tag hits in icache")

    val s1_hit_way = PriorityEncoder(s1_tag_hit_oh)

    when (s1_tag_hit) {
      data_array.readPorts(0).read(s1_set)
    }

    when (s1_valid && !s1_tag_hit && !miss_busy) {
      miss_busy := true.B
      miss_set := s1_set
      miss_tag := s1_paddr_tag
      miss_victim := lfsr(log2Ceil(nWays)-1, 0)

      val req_addr = p.blockAlign(s1_paddr)
      miss_req_addr := req_addr

      // Send memory request... assume single cycle response for now
      io.mem.req.valid := true.B
      io.mem.req.bits.addr := req_addr
    }

    when (io.mem.resp.valid && miss_busy) {
      val replace_tag = Wire(VecTagEntry(nWays))
      replace_tag := DontCare
      replace_tag(miss_victim).tag   := miss_tag
      replace_tag(miss_victim).valid := true.B

      val write_mask = Wire(Vec.fill(nWays)(Bool()))
      write_mask.zipWithIndex.foreach((wm, idx) => {
        wm := idx.U === miss_victim
      })

      tag_array.writePorts(0).write(miss_set, replace_tag, write_mask)

      val replace_data = Wire(VecDataEntry(nWays))
      replace_data := DontCare
      replace_data(miss_victim) := Cat(io.mem.resp.bits.lineWords.reverse)
      data_array.writePorts(0).write(miss_set, replace_data, write_mask)

      miss_busy := false.B
    }

    // -----------------------------------------------------------------------
    // Stage 2
    // - Send response to core
    // -----------------------------------------------------------------------
    val s2_valid = RegInit(false.B)
    s2_valid := s1_valid && !io.core.s1_kill

    val s2_hit = Wire(Bool())
    s2_hit := s2_valid && RegNext(s1_tag_hit) && !io.core.s2_kill

    val s2_set = RegNext(s1_set)
    val s2_vaddr = RegNext(s1_vaddr)
    val s2_hit_way = RegNext(s1_hit_way)
    val s2_insts = Wire(Vec.fill(p.icacheFetchInstCount)(UInt(p.instBits.W)))
    val s2_data_array_out = Wire(UInt((lineBytes*8).W))
    s2_data_array_out := data_array.readPorts(0).data(s2_hit_way)

    dontTouch(s2_data_array_out)

    // |      CL                                  |
    // | inst 0 | inst 1 | inst 2 | ... | inst 15 |
    val fetchBytesOffset = log2Ceil(p.fetchBytes)
    val s2_vaddr_fetch_group = s2_vaddr(lineOffBits-1, fetchBytesOffset)
    val s2_vaddr_fetch_group_byte_offset = s2_vaddr_fetch_group << log2Ceil(p.fetchBytes)
    val s2_vaddr_fetch_group_bit_offset = s2_vaddr_fetch_group_byte_offset << 3

    for (i <- 0 until p.icacheFetchInstCount) {
      val inst_shamt = (i << log2Ceil(p.instBits)).U
      s2_insts(i) := s2_data_array_out >> (s2_vaddr_fetch_group_bit_offset + inst_shamt)
    }

    io.core.s2_valid := s2_hit
    io.core.s2_insts := s2_insts
  }

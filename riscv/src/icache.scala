package riscv

import hdl._


case class MagicMemReq(
  addr: UInt
) extends Bundle[MagicMemReq]

case class MagicMemResp(
  lineWords: Vec[UInt]
) extends Bundle[MagicMemResp]

case class MagicMemIf(
  req: Valid[MagicMemReq],
  resp: MagicMemResp
) extends Bundle[MagicMemIf]

object MagicMemReq:
  def apply(p: CoreParams): MagicMemReq =
    MagicMemReq(addr = UInt(p.pcBits.W))

object MagicMemResp:
  def apply(p: CoreParams): MagicMemResp =
    MagicMemResp(lineWords = Vec.fill(p.ic.cacheLineBytes/4)(UInt(32.W)))

object MagicMemIf:
  def apply(p: CoreParams): MagicMemIf =
    MagicMemIf(
      req  =   Valid(MagicMemReq (p)),
      resp = Flipped(MagicMemResp(p))
    )

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

class ICache(
  p: CoreParams,
) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(ICacheBundle(p))

  val ic = p.ic

  body {
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

    val tag_array = SRAM(VecTagEntry(nWays), nSets)
                        (reads = 1, writes = 1, readwrites = 0)

    val data_array = SRAM(Vec.fill(nWays)(UInt(ic.cacheLineBytes.W)), nSets)(
      reads = 1, writes = 1, readwrites = 0)

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    def setIdx(addr: UInt): UInt =
      addr(lineOffBits + setIdxBits - 1, lineOffBits)

    def tagOf(addr: UInt): UInt =
      addr(p.pcBits-1, lineOffBits + setIdxBits)

   //  def wordOffsetInLine(addr: UInt): UInt =
   //    addr(lineOffBits-1, 2) // word offset within the line (0..wordsPerLine-1)

   //  def groupIdx(addr: UInt): UInt =
   //    // row inside each bank = wordOffset / banks
   //    wordOffsetInLine(addr) >> bankBits.U

    // ---- Miss/Refill path (single outstanding) ----
    // You said backing mem is 1-cycle magic. Model as a side module.
    // Replace this with whatever "magic memory" you already have.
// val mem = Module(new MagicIMem(p, depthWords = 1 << 16)) // example depth
// mem.io.req.valid := false.B
// mem.io.req.bits.addr := DontCare
    io.mem.req.valid := false.B
    io.mem.req.bits.addr := DontCare

    io.core.s2_valid := false.B
    io.core.s2_insts := DontCare
 
    val missBusy      = RegInit(false.B)
    val miss_set      = Reg(UInt(setIdxBits.W))
    val miss_tag      = Reg(UInt(tagBits.W))
    val miss_victim   = Reg(UInt(log2Ceil(nWays).W))

    val s0_valid = io.core.s0_vaddr.valid && !missBusy
    val s0_vaddr = io.core.s0_vaddr.bits

    val s0_set = setIdx(s0_vaddr)

    val s1_tags = Wire(VecTagEntry(nWays))
    s1_tags := DontCare

    when (s0_valid) {
      s1_tags := tag_array.readPorts(0).read(s0_set)
    }

    val s1_valid = RegNext(s0_valid) && !io.core.s1_kill && io.core.s1_paddr.valid

    val tag_hit_vec = s1_tags.map(entry => {
      entry.valid && (entry.tag === tagOf(io.core.s1_paddr.bits)) && s1_valid
    })

    val tag_hit = tag_hit_vec.reduce(_ || _)


   //  // Read all ways' tags + all ways' banked data in s0, register into s1.
   //  val s1_valid = RegNext(s0_valid)
   //  val s1_paddr = RegNext(s0_vaddr)   // baremetal => PA = VA
   //  val s1_set   = RegNext(s0_set)
   //  val s1_group = RegNext(s0_group)

   //  val s1_tags = Reg(Vec.fill(ways)(TagEntry(tagBits))))
   //  val s1_data = Reg(Vec.fill(ways)(Vec.fill(banks)(UInt(32.W)))))

   //  // s0 "read" (combinational from Reg arrays), captured into s1 regs
   //  for (w <- 0 until ways) {
   //    s1_tags(w) := tagArray(w)(s0_set)
   //    for (b <- 0 until banks) {
   //      s1_data(w)(b) := dataArray(w)(b)(s0_set)(s0_group)
   //    }
   //  }

   //  // s1: tag compare using PA tag
   //  val s1_ptag = tagOf(s1_paddr)
   //  val hitVec  = Wire(Vec.fill(ways)(Bool())))

   //  for (w <- 0 until ways) {
   //    hitVec(w) := s1_valid && s1_tags(w).valid && (s1_tags(w).tag === s1_ptag)
   //  }

   //  val hit = hitVec.reduce(_ || _)

   //  // pick first hit way (ok because duplicates shouldn’t happen)

   //  val hitOH = oneHotFirst(hitVec)

   //  // select data for hit way
   //  val s1_hitWords = Wire(Vec.fill(banks)(UInt(32.W)))))
   //  for (b <- 0 until banks) {
   //    s1_hitWords(b) := 0.U
   //    for (w <- 0 until ways) {
   //      when(hitOH(w)) { s1_hitWords(b) := s1_data(w)(b) }
   //    }
   //  }

   //  // Miss detect and refill request issue (at s1)
   //  when (s1_valid && !hit && !missBusy) {
   //    missBusy := true.B
   //    miss_set := s1_set
   //    miss_tag := s1_ptag
   //    miss_group := s1_group

   //    // random victim
   //    miss_victim := lfsr(log2Ceil(ways)-1, 0)
   //    miss_req_addr := p.blockAlign(s1_paddr)

   //    mem.io.req.valid := true.B
   //    mem.io.req.bits.addr := p.blockAlign(s1_paddr)
   //  }

   //  // refill returns 1 cycle later
   //  val refill_valid = mem.io.resp.valid && missBusy

   //  when (refill_valid) {
   //    // fill tag
   //    tagArray(miss_victim)(miss_set).valid := true.B
   //    tagArray(miss_victim)(miss_set).tag   := miss_tag

   //    // fill data banks from full line
   //    for (word <- 0 until wordsPerLine) {
   //      val b = word % banks
   //      val r = word / banks
   //      dataArray(miss_victim)(b)(miss_set)(r) := mem.io.resp.bits.lineWords(word)
   //    }

   //    missBusy := false.B
   //  }

   //  // s2 output:
   //  // - on hit: register s1_hitWords
   //  // - on refill completion: bypass the refill line words for the requested group
   //  val s2_valid = RegInit(false.B)
   //  val s2_words = Reg(Vec.fill(banks)(UInt(32.W)))))

   //  val hit_to_s2 = s1_valid && hit && !missBusy
   //  when (hit_to_s2) {
   //    s2_valid := true.B
   //    for (b <- 0 until banks) { s2_words(b) := s1_hitWords(b) }
   //  } .elsewhen (refill_valid) {
   //    // bypass: words for the requested group are contiguous banks
   //    s2_valid := true.B
   //    val base = miss_group * banks.U
   //    for (b <- 0 until banks) {
   //      s2_words(b) := mem.io.resp.bits.lineWords(base + b.U)
   //    }
   //  } .otherwise {
   //    s2_valid := false.B
   //  }

   //  // Response formatting
   //  // Your resp inst width is xlenBits. Put inst in low 32 bits.
   //  for (i <- 0 until banks) {
   //    io.resp.insts(i).valid := s2_valid
   //    if (p.xlenBits == 32) {
   //      io.resp.insts(i).bits := s2_words(i)
   //    } else {
   //      io.resp.insts(i).bits := Cat(0.U((p.xlenBits-32).W), s2_words(i))
   //    }
   //  }

   //  // If you add stall:
   //  // io.resp.stall := missBusy || (s1_valid && !hit)  // “hold PC until data returns”
  }// 

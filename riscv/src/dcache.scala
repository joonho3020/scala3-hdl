package riscv

import hdl._
import CoreConstants._

case class DCacheReq(
  vaddr: UInt,
  data: UInt,
  size: HWEnum[MemWidth],
  signed: Bool,
  tpe:  HWEnum[MemOp],
  tag: UInt
) extends Bundle[DCacheReq]

case class DCacheResp(
  data: UInt,
  tag: UInt,
  tpe:  HWEnum[MemOp],
) extends Bundle[DCacheResp]

case class DCacheIf(
  s0_req: Decoupled[DCacheReq],

  s1_kill: Bool,
  s1_paddr: Valid[UInt],

  s2_kill: Bool,
  s2_resp: Valid[DCacheResp]
) extends Bundle[DCacheIf]

object DCacheReq:
  def apply(p: CoreParams, tagBits: Int): DCacheReq =
    DCacheReq(
      vaddr = Input(UInt(p.xlenBits.W)),
      data = Input(UInt(p.xlenBits.W)),
      size = Input(HWEnum(MemWidth)),
      signed = Input(Bool()),
      tpe = Input(HWEnum(MemOp)),
      tag = Input(UInt(tagBits.W))
    )

object DCacheResp:
  def apply(p: CoreParams, tagBits: Int): DCacheResp =
    DCacheResp(
      data = Output(UInt(p.xlenBits.W)),
      tag = Output(UInt(tagBits.W)),
      tpe = Output(HWEnum(MemOp))
    )

object DCacheIf:
  def apply(p: CoreParams, tagBits: Int): DCacheIf =
    DCacheIf(
      s0_req = Flipped(Decoupled(DCacheReq(p, tagBits))),

      s1_kill = Input(Bool()),
      s1_paddr = Input(Valid(UInt(p.paddrBits.W))),

      s2_kill = Input(Bool()),
      s2_resp = Valid(DCacheResp(p, tagBits))
    )

case class DCacheBundle(
  core: DCacheIf,
  mem: MagicMemIf
) extends Bundle[DCacheBundle]

object DCacheBundle:
  def apply(p: CoreParams, tagBits: Int): DCacheBundle =
    DCacheBundle(
      core = DCacheIf(p, tagBits),
      mem = MagicMemIf(p)
    )

class DCache(p: CoreParams, tagBits: Int) extends Module with CoreCacheable(p):
  given Module = this
  val io = IO(DCacheBundle(p, tagBits))

  body {
    val dc = p.dc
    val nSets = dc.nSets
    val nWays = dc.nWays
    assert((nSets & (nSets-1)) == 0)
    assert((nWays & (nWays-1)) == 0)
    assert(p.memLineBytes == dc.cacheLineBytes)

    val lineBytes = dc.cacheLineBytes
    val lineBits = lineBytes * 8
    val lineOffBits = log2Ceil(lineBytes)
    val setIdxBits = log2Ceil(nSets)
    val cacheTagBits = p.xlenBits - (setIdxBits + lineOffBits)
    val tagOffBits = setIdxBits + lineOffBits

    def VecTagEntry(entries: Int): Vec[TagEntry] =
      Vec.fill(entries)(TagEntry(cacheTagBits))

    def setIdx(addr: UInt): UInt =
      addr(lineOffBits + setIdxBits - 1, lineOffBits)

    def tagOf(addr: UInt): UInt =
      addr(p.xlenBits-1, lineOffBits + setIdxBits)

    def byteOff(addr: UInt): UInt =
      addr(lineOffBits-1, 0)

    def blockAlign(addr: UInt): UInt =
      ~(~addr | (lineBytes-1).U)

    def sizeBytes(size: HWEnum[MemWidth]): UInt =
      import MemWidth._
      val ret = Wire(UInt(4.W))
      switch (size) {
        is(B.EN) { ret := 1.U }
        is(H.EN) { ret := 2.U }
        is(W.EN) { ret := 4.U }
        is(D.EN) { ret := 8.U }
        default  { ret := 1.U }
      }
      ret

    def storeMask(off: UInt, size: HWEnum[MemWidth]): Vec[Bool] =
      val ret = Wire(Vec.fill(lineBytes)(Bool()))
      val mask = ((1.U((lineBytes + 1).W) << sizeBytes(size)) - 1.U)(lineBytes-1,0) << off
      for (i <- 0 until lineBytes) {
        ret(i) := mask(i)
      }
      ret

    def storeBitMask(mask: UInt): UInt =
      Cat((0 until lineBytes).map(i => Fill(8, mask(i))).reverse)

    def storeShift(data: UInt, off: UInt): UInt =
      {
        val ext = Cat(Seq(0.U((lineBits - p.xlenBits).W), data))
        (ext << (off << 3))(lineBits-1, 0)
      }

    def storeMerge(data: UInt, vaddr: UInt, size: HWEnum[MemWidth], cl: UInt): UInt =
      val offset = byteOff(vaddr)
      val wdata = storeShift(data, offset)
      val wmask = storeMask(offset, s1_req.bits.size)
      val wmask_bits = storeBitMask(Cat(wmask.reverse))
      (cl & ~wmask_bits) | (wdata & wmask_bits)

    def loadData(line: UInt, off: UInt, size: HWEnum[MemWidth], signed: Bool): UInt =
      {
        val shamt = off << 3
        val raw8  = (line >> shamt)(7,0)
        val raw16 = (line >> shamt)(15,0)
        val raw32 = (line >> shamt)(31,0)
        val raw64 = (line >> shamt)(63,0)
        val load_b = Cat(Seq(Fill(p.xlenBits - 8, Mux(signed, raw8(7), 0.U)   ), raw8 ))
        val load_h = Cat(Seq(Fill(p.xlenBits - 16, Mux(signed, raw16(15), 0.U)), raw16))
        val load_w = Cat(Seq(Fill(p.xlenBits - 32, Mux(signed, raw32(31), 0.U)), raw32))
        val load_d = Cat(Seq(Fill(p.xlenBits - 64, Mux(signed, raw64(63), 0.U)), raw64))
        val out = Wire(UInt(p.xlenBits.W))
        out := 0.U
        switch(size) {
          is(MemWidth.B.EN) { out := load_b }
          is(MemWidth.H.EN) { out := load_h }
          is(MemWidth.W.EN) { out := load_w }
          is(MemWidth.D.EN) { out := load_d }
        }
        out
      }

    val tag_array = SRAM(VecTagEntry(nWays), nSets)
                        (reads = 1, writes = 1, readwrites = 0, masked = true)

    val data_array = Seq.fill(nWays)(
                     SRAM(UInt(lineBits.W), nSets)
                         (reads = 1, writes = 1, readwrites = 0, masked = true))

    val dirty_array = RegInit(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))
    val valid_array = RegInit(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    enum MissState:
      case Idle,
           EvictReq, EvictWait,
           RefillReq, RefillWait,
           Response

    import MissState._

    def is_busy(state: HWEnum[MissState]): Bool =
      (state =/= Idle.EN && state =/= Response.EN)


    val mstate = RegInit(Idle.EN)

    val s1_miss        = Wire(Bool())
    val miss_set       = Reg(UInt(setIdxBits.W))
    val miss_tag       = Reg(UInt(tagBits.W))
    val miss_victim    = Reg(UInt(log2Ceil(nWays).W))
    val miss_req       = Reg(DCacheReq(p, tagBits))

    val victim_tag   = Reg(UInt(cacheTagBits.W))
    val victim_valid = Reg(Bool())
    val victim_dirty = Reg(Bool())
    val victim_line  = Reg(UInt(lineBits.W))
    val refill_line  = Reg(UInt(lineBits.W))

    // -----------------------------------------------------------------------
    // S0
    // - Tag lookup
    // -----------------------------------------------------------------------

    val s0_req = Wire(Valid(DCacheReq(p, tagBits)))
    s0_req.valid := io.core.s0_req.fire
    s0_req.bits  := io.core.s0_req.bits

    val s0_set = setIdx(s0_req.bits.vaddr)
    when (s0_req.valid) {
      tag_array.readPorts(0).read(s0_set)
    }

    io.core.s0_req.ready := is_busy(mstate) && !s1_miss

    // -----------------------------------------------------------------------
    // S1
    // - Tag matching
    //   - Hit
    //     - Reads: read data array and return result in s2
    //     - Writes: write data array, mark dirty bit
    //   - Miss
    //     - Block incoming requests (blocking cache for now)
    //     - Send request to backing memory
    //     - When response comes back,
    //       - Replacement policy
    //       - Clean dirty bit
    // -----------------------------------------------------------------------

    val s1_req = RegNext(s0_req)
    val s1_set = setIdx(s1_req.bits.vaddr)
    val s1_paddr = io.core.s1_paddr.bits
    val s1_paddr_tag = tagOf(s1_paddr)
    val s1_tags = tag_array.readPorts(0).data

    val s1_tag_hit_vec = s1_tags.zipWithIndex.map((entry, way) => {
      s1_req.valid &&
      valid_array(s1_set)(way) &&
      (entry.tag === s1_paddr_tag) &&
      io.core.s1_paddr.valid
    })

    val s1_tag_hit = s1_tag_hit_vec.reduce(_ || _)
    val s1_tag_hit_oh = Cat(s1_tag_hit_vec.reverse).asOH
    val s1_tag_hit_cnt = PopCount(s1_tag_hit_oh)
    Assert(s1_tag_hit_cnt <= 1.U, "Multiple tag hits in icache")

    val s1_hit_way = PriorityEncoder(s1_tag_hit_oh)

    when (s1_tag_hit) {
      switch (s1_req.bits.tpe) {
        is(MemOp.Ld.EN) {
          for (i <- 0 until nWays) {
            when (i.U === s1_hit_way) {
              data_array(i).readPorts(0).read(s1_set)
            }
          }
        }
        is(MemOp.St.EN) {
          val offset = byteOff(s1_req.bits.vaddr)
          val wdata = storeShift(s1_req.bits.data, offset)
          val wmask = storeMask(offset, s1_req.bits.size)

          dirty_array(s1_set)(s1_hit_way) := true.B
          for (i <- 0 until nWays) {
            when (i.U === s1_hit_way) {
              data_array(i).writePorts(0).write(s1_set, wdata, wmask)
            }
          }
        }
      }
    }

    s1_miss := s1_req.valid && !s1_tag_hit && (mstate === Idle.EN)

    when (s1_miss) {
      val victim_way = lfsr(log2Ceil(nWays)-1, 0)
      miss_set  := s1_set
      miss_tag  := s1_paddr_tag
      miss_victim := victim_way
      miss_req := s1_req.bits

      Assert(mstate === Idle.EN)

      when (dirty_array(s1_set)(victim_way)) {
          tag_array.readPorts(0).read(s1_set)
          for (i <- 0 until nWays) {
            when (i.U === victim_way) {
              data_array(i).readPorts(0).read(s1_set)
            }
          }
          mstate := EvictReq.EN
      } .otherwise {
          mstate := RefillReq.EN
      }
    }

    val mstate_prev = RegNext(mstate)
    when (mstate_prev === Idle.EN && mstate === EvictReq.EN) {
      victim_tag := s1_tags(miss_victim).tag
      for (i <- 0 until nWays) {
        when (i.U === miss_victim) {
          victim_line := data_array(i).readPorts(0).data
        }
      }
    }


    val fill_data = Wire(UInt(lineBits.W))
    fill_data := DontCare

    switch (mstate) {
      is (EvictReq.EN) {
        io.mem.req.bits.addr := Mux(mstate_prev === Idle.EN,
          blockAlign(s1_tags(miss_victim).tag << tagOffBits),
          blockAlign(victim_tag << tagOffBits))

        for (i <- 0 until nWays) {
          when (i.U === miss_victim) {
            io.mem.req.bits.data := data_array(i).readPorts(0).data
          }
        }
        io.mem.req.bits.tpe := MagicMemMsg.Write.EN 
        io.mem.req.bits.mask := ((1 << lineBytes) - 1).U

        when (io.mem.req.fire) {
          mstate := EvictWait.EN
        }
      }
      is (EvictWait.EN) {
        when (io.mem.resp.valid) {
          mstate := RefillReq.EN
        }
      }
      is (RefillReq.EN) {
        valid_array(miss_set)(miss_victim) := false.B
        dirty_array(miss_set)(miss_victim) := false.B

        io.mem.req.bits.addr := blockAlign(miss_tag << tagOffBits)
        io.mem.req.bits.data := DontCare
        io.mem.req.bits.tpe  := MagicMemMsg.Read.EN
        io.mem.req.bits.mask := DontCare
        when (io.mem.req.fire) {
          mstate := RefillWait.EN
        }
      }
      is (RefillWait.EN) {
        when (io.mem.resp.valid) {
          val replace_tag = Wire(VecTagEntry(nWays))
          replace_tag := DontCare
          replace_tag(miss_victim).tag   := miss_tag

          val write_mask = Wire(Vec.fill(nWays)(Bool()))
          write_mask.zipWithIndex.foreach((wm, idx) => {
            wm := idx.U === miss_victim
          })
          tag_array.writePorts(0).write(miss_set, replace_tag, write_mask)

          val write = miss_req.tpe === MemOp.St.EN
          val resp_data = Cat(io.mem.resp.bits.lineWords.reverse)
          val merged_wdata = storeMerge(miss_req.data,
                                        miss_req.vaddr,
                                        miss_req.size,
                                        resp_data)
          fill_data := Mux(write, merged_wdata, resp_data)
          for (i <- 0 until nWays) {
            when (i.U === miss_victim) {
              data_array(i).writePorts(0).write(miss_set, fill_data)
            }
          }

          valid_array(miss_set)(miss_victim) := true.B
          dirty_array(miss_set)(miss_victim) := write
          mstate := Response.EN
        }
      }
      is (Response.EN) {
        mstate := Idle.EN
      }
    }

    io.mem.req.valid := mstate === EvictReq.EN || mstate === RefillReq.EN

    // -----------------------------------------------------------------------
    // S2
    // - Send back response
    // -----------------------------------------------------------------------

    val s2_hit = RegNext(s1_req.valid && s1_tag_hit)
    val s2_hit_way = RegNext(s1_hit_way)
    val s2_req = RegNext(s1_req)

    val s2_miss_req = RegNext(miss_req)
    val s2_miss_fill_data = RegNext(fill_data)

    io.core.s2_resp := DontCare

    when (s2_hit && s2_req.valid) {
      io.core.s2_resp.valid := true.B
      for (i <- 0 until nWays) {
        when (i.U === s2_hit_way) {
          io.core.s2_resp.bits.data :=
            loadData(
              data_array(i).readPorts(0).data,
              byteOff(s2_req.bits.vaddr),
              s2_req.bits.size,
              s2_req.bits.signed)
        }
      }
      io.core.s2_resp.bits.tag := DontCare
      io.core.s2_resp.bits.tpe := s2_req.bits.tpe
    } .elsewhen (mstate === Response.EN) {
      io.core.s2_resp.valid := true.B
      for (i <- 0 until nWays) {
        when (i.U === s2_hit_way) {
          io.core.s2_resp.bits.data :=
            loadData(
              s2_miss_fill_data,
              byteOff(s2_miss_req.vaddr),
              s2_miss_req.size,
              s2_miss_req.signed)
        }
      }
      io.core.s2_resp.bits.tag := DontCare
      io.core.s2_resp.bits.tpe := s2_miss_req.tpe
    }
  }

package riscv_inorder

import hdl.core._
import hdl.util._
import hdl.elaboration._

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
      vaddr = UInt(p.xlenBits.W),
      data = UInt(p.xlenBits.W),
      size = HWEnum(MemWidth),
      signed = Bool(),
      tpe = HWEnum(MemOp),
      tag = UInt(tagBits.W)
    )

object DCacheResp:
  def apply(p: CoreParams, tagBits: Int): DCacheResp =
    DCacheResp(
      data = UInt(p.xlenBits.W),
      tag = UInt(tagBits.W),
      tpe = HWEnum(MemOp)
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

class DCache(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

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
  val tagBits = p.xlenBits - (setIdxBits + lineOffBits)
  val tagOffBits = setIdxBits + lineOffBits

  val io = IO(DCacheBundle(p, tagBits))

  body {
    def VecTagEntry(entries: Int): Vec[UInt] =
      Vec.fill(entries)(UInt(tagBits.W))

    def VecDataEntry(bytes: Int): Vec[UInt] =
      Vec.fill(bytes)(UInt(8.W))

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
        ret(i) := mask(i).asBool
      }
      ret

    def storeBitMask(mask: UInt): UInt =
      val bmask = Wire(UInt(lineBits.W))
      bmask := Cat((0 until lineBytes).map(i => Fill(8, mask(i))).reverse)
      bmask

    def storeShift(data: UInt, off: UInt): UInt =
      val ret = Wire(UInt(lineBits.W))
      ret := data << (off << 3)
      ret

    def storeMerge(data: UInt, vaddr: UInt, size: HWEnum[MemWidth], cl: UInt): UInt =
      val offset = byteOff(vaddr)
      val wdata = storeShift(data, offset)
      val wmask = storeMask(offset, size)

      // NOTE: Doing wmask_bits = storeBitMask(Cat(wmask.reverse))
      // increases the verilog generation time significantly (i.e. in firtool, not our stuff)
      // perhaps there is some bitwidth inference pass pathology...?
      val wmask_uint = Wire(UInt(lineBytes.W))
      wmask_uint := Cat(wmask.reverse)
      val wmask_bits = storeBitMask(wmask_uint)

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
        val load_d = raw64
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
                     SRAM(VecDataEntry(lineBytes), nSets)
                         (reads = 1, writes = 1, readwrites = 0, masked = true))

    val dirty_array = Reg(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))
    val valid_array = Reg(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))

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

    val victim_tag   = Reg(UInt(tagBits.W))
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

    io.core.s0_req.ready := !is_busy(mstate) && !s1_miss

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
      (entry === s1_paddr_tag) &&
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
          val wdata_vec = Splice(wdata, 8)
          val wmask = storeMask(offset, s1_req.bits.size)

          dirty_array(s1_set)(s1_hit_way) := true.B
          for (i <- 0 until nWays) {
            when (i.U === s1_hit_way) {
              data_array(i).writePorts(0).write(s1_set, wdata_vec, wmask)
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
      victim_tag := s1_tags(miss_victim)
      for (i <- 0 until nWays) {
        when (i.U === miss_victim) {
          victim_line := Cat(data_array(i).readPorts(0).data.reverse)
        }
      }
    }


    val fill_data = Wire(UInt(lineBits.W))
    fill_data := DontCare

    io.mem.req.valid := mstate === EvictReq.EN || mstate === RefillReq.EN
    io.mem.req.bits  := DontCare


    switch (mstate) {
      is (EvictReq.EN) {
        io.mem.req.bits.addr := Mux(mstate_prev === Idle.EN,
          (s1_tags(miss_victim) << tagOffBits) | (miss_set << lineOffBits),
          (victim_tag << tagOffBits) | (miss_set << lineOffBits))

        val fresh_evict_data = Wire(UInt(lineBits.W))
        fresh_evict_data := 0.U
        for (i <- 0 until nWays) {
          when (i.U === miss_victim) {
            fresh_evict_data := Cat(data_array(i).readPorts(0).data.reverse)
          }
        }
        val evict_data = Mux(mstate_prev === Idle.EN, fresh_evict_data, victim_line)
        io.mem.req.bits.data := Splice(evict_data, p.memLineBytes/p.memLineWords * 8)
        io.mem.req.bits.tpe := MagicMemMsg.Write.EN
        io.mem.req.bits.mask := ~0.U(lineBytes.W)

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

        io.mem.req.bits.addr := (miss_tag << tagOffBits) | (miss_set << lineOffBits)
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
          replace_tag(miss_victim) := miss_tag
          dontTouch(replace_tag)

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
          val wdata_vec = Splice(fill_data, Seq.fill(lineBytes)(8))

          val wmask = Wire(Vec.fill(lineBytes)(Bool()))
          wmask.foreach(_ := true.B)

          for (i <- 0 until nWays) {
            when (i.U === miss_victim) {
              data_array(i).writePorts(0).write(miss_set, wdata_vec, wmask)
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

    // -----------------------------------------------------------------------
    // S2
    // - Send back response
    // -----------------------------------------------------------------------

    val s2_hit = RegNext(s1_req.valid && s1_tag_hit)
    val s2_hit_way = RegNext(s1_hit_way)
    val s2_req = RegNext(s1_req)

    val s2_miss_req = RegNext(miss_req)
    val s2_miss_fill_data = RegNext(fill_data)

    io.core.s2_resp.valid := false.B
    io.core.s2_resp.bits := DontCare

    when (s2_hit && s2_req.valid) {
      io.core.s2_resp.valid := true.B
      for (i <- 0 until nWays) {
        when (i.U === s2_hit_way) {
          io.core.s2_resp.bits.data :=
            loadData(
              Cat(data_array(i).readPorts(0).data.reverse),
              byteOff(s2_req.bits.vaddr),
              s2_req.bits.size,
              s2_req.bits.signed)
        }
      }
      io.core.s2_resp.bits.tag := DontCare
      io.core.s2_resp.bits.tpe := s2_req.bits.tpe
    } .elsewhen (mstate === Response.EN) {
      io.core.s2_resp.valid := true.B
      io.core.s2_resp.bits.data :=
        loadData(
          s2_miss_fill_data,
          byteOff(s2_miss_req.vaddr),
          s2_miss_req.size,
          s2_miss_req.signed)
      io.core.s2_resp.bits.tag := DontCare
      io.core.s2_resp.bits.tpe := s2_miss_req.tpe
    }

    when (reset.asBool) {
      for (i <- 0 until nSets) {
        for (j <- 0 until nWays) {
          dirty_array(i)(j) := false.B
          valid_array(i)(j) := false.B
        }
      }
    }

    dontTouch(dirty_array)
    dontTouch(valid_array)
    dontTouch(s1_miss)


    dontTouch(mstate)
    dontTouch(miss_set)
    dontTouch(miss_tag)
    dontTouch(miss_victim)
    dontTouch(miss_req)

    dontTouch(victim_tag)
    dontTouch(victim_valid)
    dontTouch(victim_dirty)
    dontTouch(victim_line)
    dontTouch(refill_line)

    dontTouch(fill_data)

    dontTouch(s0_req)
    dontTouch(s2_req)

    dontTouch(s2_hit)
    dontTouch(s2_hit_way)
    dontTouch(s2_req)
    dontTouch(s2_miss_req)
    dontTouch(s2_miss_fill_data)

    dontTouch(io.core.s2_resp)
  }

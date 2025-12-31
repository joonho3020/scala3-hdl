package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import riscv_inorder.CoreConstants._

// MSHR (Miss Status Holding Register) Entry
case class MSHREntry(
  valid: Bool,

  // Miss information
  addr: UInt,                      // Block-aligned address
  tag: UInt,
  set_idx: UInt,
  victim_way: UInt,

  // Request tracking
  req_is_load: Bool,
  req_is_store: Bool,
  req_size: HWEnum[MemWidth],
  req_signed: Bool,
  req_data: UInt,

  // Refill state
  refill_data: Vec[UInt],
  refill_valid: Vec[Bool],

  // State tracking
  state: HWEnum[MSHRState],

  // Metadata
  old_tag: UInt,
  old_dirty: Bool,
  old_valid: Bool,
) extends Bundle[MSHREntry]

object MSHREntry:
  def apply(p: CoreParams): MSHREntry =
    val dc = p.dc
    val lineBytes = dc.cacheLineBytes
    val beatsPerLine = lineBytes / 8  // 8 bytes per beat

    MSHREntry(
      valid = Bool(),
      addr = UInt(p.paddrBits.W),
      tag = UInt((p.paddrBits - log2Ceil(dc.nSets) - log2Ceil(lineBytes)).W),
      set_idx = UInt(log2Ceil(dc.nSets).W),
      victim_way = UInt(log2Ceil(dc.nWays).W),

      req_is_load = Bool(),
      req_is_store = Bool(),
      req_size = HWEnum(MemWidth),
      req_signed = Bool(),
      req_data = UInt(p.xlenBits.W),

      refill_data = Vec.fill(beatsPerLine)(UInt((8*8).W)),  // 8 bytes per beat
      refill_valid = Vec.fill(beatsPerLine)(Bool()),

      state = HWEnum(MSHRState),

      old_tag = UInt((p.paddrBits - log2Ceil(dc.nSets) - log2Ceil(lineBytes)).W),
      old_dirty = Bool(),
      old_valid = Bool()
    )

// MSHR State Machine
enum MSHRState:
  case Idle,
       MetaRead,
       MetaResp,
       EvictReq,
       EvictWait,
       RefillReq,
       RefillWait,
       RefillResp,
       WriteCache,
       Drain

// Non-Blocking D-Cache Bundle (extends basic DCacheIf)
case class NBDCacheBundle(
  core: DCacheIf,
  mem: MagicMemIf,

  // Additional signals for non-blocking operation
  mshr_full: Bool,
  hit_under_miss: Bool,            // Hit while miss pending
) extends Bundle[NBDCacheBundle]

object NBDCacheBundle:
  def apply(p: CoreParams, tagBits: Int): NBDCacheBundle =
    NBDCacheBundle(
      core = DCacheIf(p, tagBits),
      mem = MagicMemIf(p),
      mshr_full = Output(Bool()),
      hit_under_miss = Output(Bool())
    )

class NonBlockingDCache(p: CoreParams) extends Module with CoreCacheable(p):
  given Module = this

  val dc = p.dc
  val nSets = dc.nSets
  val nWays = dc.nWays
  val nMSHRs = dc.mshrs

  assert((nSets & (nSets-1)) == 0)
  assert((nWays & (nWays-1)) == 0)
  assert(p.memLineBytes == dc.cacheLineBytes)

  val lineBytes = dc.cacheLineBytes
  val lineBits = lineBytes * 8
  val lineOffBits = log2Ceil(lineBytes)
  val setIdxBits = log2Ceil(nSets)
  val tagBits = p.paddrBits - (setIdxBits + lineOffBits)
  val tagOffBits = setIdxBits + lineOffBits

  val io = IO(NBDCacheBundle(p, tagBits))

  body {
    import MSHRState._

    // Reuse helper functions from blocking D-cache
    def VecTagEntry(entries: Int): Vec[UInt] =
      Vec.fill(entries)(UInt(tagBits.W))

    def VecDataEntry(bytes: Int): Vec[UInt] =
      Vec.fill(bytes)(UInt(8.W))

    def setIdx(addr: UInt): UInt =
      addr(lineOffBits + setIdxBits - 1, lineOffBits)

    def tagOf(addr: UInt): UInt =
      addr(p.paddrBits-1, lineOffBits + setIdxBits)

    def byteOff(addr: UInt): UInt =
      addr(lineOffBits-1, 0)

    def blockAlign(addr: UInt): UInt =
      ~(~addr | (lineBytes-1).U)

    // ========================================================================
    // Cache Arrays (same as blocking cache)
    // ========================================================================

    val tag_array = SRAM(VecTagEntry(nWays), nSets)
                        (reads = 1, writes = 1, readwrites = 0, masked = true)

    val data_array = Seq.fill(nWays)(
                     SRAM(VecDataEntry(lineBytes), nSets)
                         (reads = 1, writes = 1, readwrites = 0, masked = true))

    val dirty_array = Reg(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))
    val valid_array = Reg(Vec.fill(nSets)(Vec.fill(nWays)(false.B)))

    val lfsr = RegInit(1.U(16.W))
    lfsr := Cat(Seq(lfsr(14,0), lfsr(15) ^ lfsr(13) ^ lfsr(12) ^ lfsr(10)))

    // ========================================================================
    // MSHRs
    // ========================================================================

    val mshrs = Reg(Vec.fill(nMSHRs)(MSHREntry(p)))

    // Initialize MSHRs
    for (i <- 0 until nMSHRs) {
      when (reset.asBool) {
        mshrs(i).valid := false.B
        mshrs(i).state := Idle.EN
      }
    }

    // MSHR allocation logic
    val mshr_alloc_idx = Wire(Valid(UInt(log2Ceil(nMSHRs).W)))
    mshr_alloc_idx.valid := false.B
    mshr_alloc_idx.bits := DontCare

    for (i <- 0 until nMSHRs) {
      when (!mshrs(i).valid && !mshr_alloc_idx.valid) {
        mshr_alloc_idx.valid := true.B
        mshr_alloc_idx.bits := i.U
      }
    }

    val any_mshr_valid = mshrs.map(_.valid).reduce(_ || _)
    io.mshr_full := !mshr_alloc_idx.valid

    // ========================================================================
    // S0: Tag Lookup
    // ========================================================================

    val s0_req = Wire(Valid(DCacheReq(p, tagBits)))
    s0_req.valid := io.core.s0_req.fire
    s0_req.bits  := io.core.s0_req.bits

    val s0_set = setIdx(s0_req.bits.vaddr)
    when (s0_req.valid) {
      tag_array.readPorts(0).read(s0_set)
    }

    // Can accept new requests if:
    // 1. Not all MSHRs full, OR
    // 2. This is a hit (determined in S1)
    io.core.s0_req.ready := mshr_alloc_idx.valid || !any_mshr_valid

    // ========================================================================
    // S1: Tag Match & MSHR Check
    // ========================================================================

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
    val s1_hit_way = PriorityEncoder(s1_tag_hit_oh)

    // Check if any MSHR is already handling this cache line
    val s1_mshr_hit = Wire(Bool())
    val s1_mshr_hit_idx = Wire(UInt(log2Ceil(nMSHRs).W))
    s1_mshr_hit := false.B
    s1_mshr_hit_idx := DontCare

    for (i <- 0 until nMSHRs) {
      when (mshrs(i).valid &&
            (mshrs(i).set_idx === s1_set) &&
            (mshrs(i).tag === s1_paddr_tag)) {
        s1_mshr_hit := true.B
        s1_mshr_hit_idx := i.U
      }
    }

    val s1_hit = s1_tag_hit
    val s1_miss = s1_req.valid && !s1_tag_hit && !s1_mshr_hit

    io.hit_under_miss := s1_tag_hit && any_mshr_valid

    // ========================================================================
    // S1: Hit Path (same as blocking cache)
    // ========================================================================

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
          val wdata = Wire(UInt(lineBits.W))
          wdata := s1_req.bits.data << (offset << 3)
          val wdata_vec = Splice(wdata, 8)

          // TODO: Generate proper byte mask based on size
          val wmask = Wire(Vec.fill(lineBytes)(Bool()))
          wmask.foreach(_ := true.B)  // Simplified for now

          dirty_array(s1_set)(s1_hit_way) := true.B
          for (i <- 0 until nWays) {
            when (i.U === s1_hit_way) {
              data_array(i).writePorts(0).write(s1_set, wdata_vec, wmask)
            }
          }
        }
      }
    }

    // ========================================================================
    // S1: Miss Path - Allocate MSHR
    // ========================================================================

    when (s1_miss && mshr_alloc_idx.valid) {
      val mshr_idx = mshr_alloc_idx.bits
      val victim_way = lfsr(log2Ceil(nWays)-1, 0)

      mshrs(mshr_idx).valid := true.B
      mshrs(mshr_idx).addr := blockAlign(s1_paddr)
      mshrs(mshr_idx).tag := s1_paddr_tag
      mshrs(mshr_idx).set_idx := s1_set
      mshrs(mshr_idx).victim_way := victim_way

      mshrs(mshr_idx).req_is_load := s1_req.bits.tpe === MemOp.Ld.EN
      mshrs(mshr_idx).req_is_store := s1_req.bits.tpe === MemOp.St.EN
      mshrs(mshr_idx).req_size := s1_req.bits.size
      mshrs(mshr_idx).req_signed := s1_req.bits.signed
      mshrs(mshr_idx).req_data := s1_req.bits.data

      mshrs(mshr_idx).state := MetaRead.EN

      // Read victim metadata
      tag_array.readPorts(0).read(s1_set)
      for (i <- 0 until nWays) {
        when (i.U === victim_way) {
          data_array(i).readPorts(0).read(s1_set)
        }
      }
    }

    // ========================================================================
    // MSHR Memory Request Arbitration
    // ========================================================================

    // Find MSHR requesting memory (simple priority arbitration)
    val mem_req_mshr_idx = Wire(Valid(UInt(log2Ceil(nMSHRs).W)))
    mem_req_mshr_idx.valid := false.B
    mem_req_mshr_idx.bits := DontCare

    for (i <- 0 until nMSHRs) {
      when (mshrs(i).valid && !mem_req_mshr_idx.valid) {
        when (mshrs(i).state === EvictReq.EN || mshrs(i).state === RefillReq.EN) {
          mem_req_mshr_idx.valid := true.B
          mem_req_mshr_idx.bits := i.U
        }
      }
    }

    // Memory request
    io.mem.req.valid := mem_req_mshr_idx.valid
    io.mem.req.bits := DontCare

    when (mem_req_mshr_idx.valid) {
      val mshr = mshrs(mem_req_mshr_idx.bits)

      when (mshr.state === EvictReq.EN) {
        // Eviction: write old dirty line
        val evict_addr = Cat(Seq(mshr.old_tag, mshr.set_idx, 0.U(lineOffBits.W)))
        io.mem.req.bits.addr := evict_addr
        io.mem.req.bits.tpe := MagicMemMsg.Write.EN

        // Read old data from cache and format for memory write
        val old_data = Wire(Vec.fill(p.memLineWords)(UInt(32.W)))
        for (i <- 0 until p.memLineWords) {
          old_data(i) := DontCare
        }
        io.mem.req.bits.data := old_data
        io.mem.req.bits.mask := Fill(lineBytes, 1.U)

      } .otherwise {
        // Refill: read new line
        io.mem.req.bits.addr := mshr.addr
        io.mem.req.bits.tpe := MagicMemMsg.Read.EN
        io.mem.req.bits.data := DontCare
        io.mem.req.bits.mask := Fill(lineBytes, 1.U)
      }
    }

    // ========================================================================
    // MSHR State Machines
    // ========================================================================

    for (i <- 0 until nMSHRs) {
      val mshr = mshrs(i)

      when (mshr.valid) {
        switch (mshr.state) {
          is (MetaRead.EN) {
            // Metadata read issued in S1, wait for response
            mshr.state := MetaResp.EN
          }

          is (MetaResp.EN) {
            // Capture victim metadata
            mshr.old_tag := s1_tags(mshr.victim_way)
            mshr.old_dirty := dirty_array(mshr.set_idx)(mshr.victim_way)
            mshr.old_valid := valid_array(mshr.set_idx)(mshr.victim_way)

            // Check if we need to evict
            when (mshr.old_valid && mshr.old_dirty) {
              mshr.state := EvictReq.EN
            } .otherwise {
              mshr.state := RefillReq.EN
            }
          }

          is (EvictReq.EN) {
            // Send eviction request to memory
            when ((mem_req_mshr_idx.bits === i.U) && io.mem.req.fire) {
              mshr.state := RefillReq.EN
            }
          }

          is (RefillReq.EN) {
            // Send refill request to memory
            when ((mem_req_mshr_idx.bits === i.U) && io.mem.req.fire) {
              mshr.state := RefillWait.EN
              // Initialize refill_valid to all false
              for (j <- 0 until (lineBytes / 8)) {
                mshr.refill_valid(j) := false.B
              }
            }
          }

          is (RefillWait.EN) {
            // Wait for refill response
            when (io.mem.resp.valid) {
              // Convert lineWords (32-bit) to refill_data (64-bit beats)
              val beatsPerLine = lineBytes / 8
              for (j <- 0 until beatsPerLine) {
                val word_lo = io.mem.resp.bits.lineWords(j * 2)
                val word_hi = io.mem.resp.bits.lineWords(j * 2 + 1)
                mshr.refill_data(j) := Cat(Seq(word_hi, word_lo))
                mshr.refill_valid(j) := true.B
              }
              mshr.state := RefillResp.EN
            }
          }

          is (RefillResp.EN) {
            mshr.state := WriteCache.EN
          }

          is (WriteCache.EN) {
            // Write refilled line to cache
            val wdata_vec = Wire(VecDataEntry(lineBytes))
            for (j <- 0 until lineBytes) {
              val beat_idx = j / 8
              val byte_off = j % 8
              wdata_vec(j) := mshr.refill_data(beat_idx)(byte_off*8+7, byte_off*8)
            }

            val wmask = Wire(Vec.fill(lineBytes)(Bool()))
            wmask.foreach(_ := true.B)

            // Write to tag and data arrays
            val new_tags = Wire(VecTagEntry(nWays))
            for (j <- 0 until nWays) {
              when (j.U === mshr.victim_way) {
                new_tags(j) := mshr.tag
              } .otherwise {
                new_tags(j) := s1_tags(j)
              }
            }

            val tag_mask = Wire(Vec.fill(nWays)(Bool()))
            for (j <- 0 until nWays) {
              tag_mask(j) := (j.U === mshr.victim_way)
            }
            tag_array.writePorts(0).write(mshr.set_idx, new_tags, tag_mask)

            for (j <- 0 until nWays) {
              when (j.U === mshr.victim_way) {
                data_array(j).writePorts(0).write(mshr.set_idx, wdata_vec, wmask)
              }
            }

            // Update metadata
            valid_array(mshr.set_idx)(mshr.victim_way) := true.B
            dirty_array(mshr.set_idx)(mshr.victim_way) := false.B

            mshr.state := Drain.EN
          }

          is (Drain.EN) {
            // Free MSHR
            mshr.valid := false.B
            mshr.state := Idle.EN
          }
        }
      }
    }

    // ========================================================================
    // S2: Response
    // ========================================================================

    val s2_hit = RegNext(s1_req.valid && s1_tag_hit)
    val s2_hit_way = RegNext(s1_hit_way)
    val s2_req = RegNext(s1_req)
    val s2_addr = RegNext(s1_req.bits.vaddr)

    // Helper: Extract and format load data based on size and signedness
    def loadData(line: UInt, offset: UInt, size: HWEnum[MemWidth], signed: Bool): UInt = {
      val byte_off = offset(log2Ceil(lineBytes)-1, 0)
      val data = Wire(UInt(p.xlenBits.W))

      // Extract bytes based on size
      val raw_data = Wire(UInt(64.W))
      switch (size) {
        is (MemWidth.B.EN) {
          raw_data := (line >> (byte_off << 3))(7, 0)
        }
        is (MemWidth.H.EN) {
          raw_data := (line >> (byte_off << 3))(15, 0)
        }
        is (MemWidth.W.EN) {
          raw_data := (line >> (byte_off << 3))(31, 0)
        }
        is (MemWidth.D.EN) {
          raw_data := (line >> (byte_off << 3))(63, 0)
        }
      }

      // Apply sign extension if needed
      switch (size) {
        is (MemWidth.B.EN) {
          data := Mux(signed && raw_data(7).asBool, Cat(Seq(Fill(56, 1.U), raw_data(7, 0))), raw_data)
        }
        is (MemWidth.H.EN) {
          data := Mux(signed && raw_data(15).asBool, Cat(Seq(Fill(48, 1.U), raw_data(15, 0))), raw_data)
        }
        is (MemWidth.W.EN) {
          data := Mux(signed && raw_data(31).asBool, Cat(Seq(Fill(32, 1.U), raw_data(31, 0))), raw_data)
        }
        is (MemWidth.D.EN) {
          data := raw_data
        }
      }

      data
    }

    io.core.s2_resp.valid := false.B
    io.core.s2_resp.bits := DontCare

    when (s2_hit && s2_req.valid) {
      io.core.s2_resp.valid := true.B

      // Get full cache line data
      val line_data = Wire(UInt(lineBits.W))
      line_data := DontCare
      for (i <- 0 until nWays) {
        when (i.U === s2_hit_way) {
          line_data := Cat(data_array(i).readPorts(0).data.reverse)
        }
      }

      // Format load response
      io.core.s2_resp.bits.data := loadData(line_data, s2_addr, s2_req.bits.size, s2_req.bits.signed)
      io.core.s2_resp.bits.tag := s2_req.bits.tag
      io.core.s2_resp.bits.tpe := s2_req.bits.tpe
    }

    // Reset logic
    when (reset.asBool) {
      for (i <- 0 until nSets) {
        for (j <- 0 until nWays) {
          dirty_array(i)(j) := false.B
          valid_array(i)(j) := false.B
        }
      }
    }

    // Debug
    dontTouch(mshrs)
    dontTouch(mshr_alloc_idx)
    dontTouch(s1_mshr_hit)
  }

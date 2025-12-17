package hdl

final class SRAMReadPortHandle[T <: HWData](
  val enable: Bool,
  val address: UInt,
  val data: T
)(using Module):
  def read(addr: UInt): T =
    enable := true.B
    address := addr
    data

  def read(addr: UInt, en: Bool): T =
    enable := en
    address := addr
    data

final class SRAMWritePortHandle[T <: HWData](
  val enable: Bool,
  val address: UInt,
  val data: T,
  val mask: Option[Vec[Bool]]
)(using Module):
  private def fillMask(bit: Boolean): Unit =
    mask.foreach { mVec =>
      var i = 0
      while i < mVec.length do
        mVec(i) := (if bit then true.B else false.B)
        i += 1
    }

  def write(addr: UInt, payload: T): Unit =
    enable := true.B
    address := addr
    data := payload
    fillMask(true)

  def write(addr: UInt, payload: T, en: Bool): Unit =
    enable := en
    address := addr
    data := payload
    fillMask(true)

  def write(addr: UInt, payload: T, maskValue: Vec[Bool]): Unit =
    enable := true.B
    address := addr
    data := payload
    mask.foreach(m => ModuleOps.connect(m, maskValue, summon[Module]))

final class SRAMReadWritePortHandle[T <: HWData](
  val enable: Bool,
  val address: UInt,
  val isWrite: Bool,
  val writeData: T,
  val readData: T,
  val mask: Option[Vec[Bool]]
)(using Module):
  private def fillMask(bit: Boolean): Unit =
    mask.foreach { mVec =>
      var i = 0
      while i < mVec.length do
        mVec(i) := (if bit then true.B else false.B)
        i += 1
    }

  def write(addr: UInt, payload: T): Unit =
    enable := true.B
    isWrite := true.B
    address := addr
    writeData := payload
    fillMask(true)

  def write(addr: UInt, payload: T, en: Bool): Unit =
    enable := en
    isWrite := true.B
    address := addr
    writeData := payload
    fillMask(true)

  def write(addr: UInt, payload: T, maskValue: Vec[Bool]): Unit =
    enable := true.B
    isWrite := true.B
    address := addr
    writeData := payload
    mask.foreach(m => ModuleOps.connect(m, maskValue, summon[Module]))

  def read(addr: UInt): T =
    enable := true.B
    isWrite := false.B
    address := addr
    fillMask(true)
    readData

  def read(addr: UInt, en: Bool): T =
    enable := en
    isWrite := false.B
    address := addr
    fillMask(true)
    readData

final class SRAMReadPorts[T <: HWData](
  en: Vec[Bool],
  addr: Vec[UInt],
  data: Vec[T]
)(using Module):
  def apply(idx: Int): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

  def apply(idx: UInt): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

final class SRAMWritePorts[T <: HWData](
  en: Vec[Bool],
  addr: Vec[UInt],
  data: Vec[T],
  mask: Option[Vec[Vec[Bool]]]
)(using Module):
  def apply(idx: Int): SRAMWritePortHandle[T] =
    SRAMWritePortHandle(
      en(idx),
      addr(idx),
      data(idx),
      mask.map(_(idx)))

  def apply(idx: UInt): SRAMWritePortHandle[T] =
    SRAMWritePortHandle(
      en(idx),
      addr(idx),
      data(idx),
      mask.map(_(idx)))

final class SRAMReadWritePorts[T <: HWData](
  en: Vec[Bool],
  addr: Vec[UInt],
  wmode: Vec[Bool],
  wdata: Vec[T],
  rdata: Vec[T],
  mask: Option[Vec[Vec[Bool]]]
)(using Module):
  def apply(idx: Int): SRAMReadWritePortHandle[T] =
    SRAMReadWritePortHandle(
      en(idx),
      addr(idx),
      wmode(idx),
      wdata(idx),
      rdata(idx),
      mask.map(_(idx)))

  def apply(idx: UInt): SRAMReadWritePortHandle[T] =
    SRAMReadWritePortHandle(
      en(idx),
      addr(idx),
      wmode(idx),
      wdata(idx),
      rdata(idx),
      mask.map(_(idx)))

final class SRAM[T <: HWData](
  data: T,
  depth: Int,
  val readPortCount: Int,
  val writePortCount: Int,
  val readwritePortCount: Int,
  val readLatency: Int = 1,
  val writeLatency: Int = 1,
  val readUnderWrite: IR.ReadUnderWrite = IR.ReadUnderWrite.Undefined,
  val masked: Boolean = false
)(using m: Module):
  val addrWidth = log2Ceil(depth)

  private val dataProto = HWAggregate.cloneData(data)
  private val memName = m.getBuilder.allocateName(Some("mem"), "mem")
  private val memImplName = s"${memName}_mem"

  private val maskWidth =
    if masked then
      dataProto match
        case v: Vec[?] => v.length
        case _ => throw new IllegalArgumentException("Masked SRAM requires Vec data")
    else 0

  m.getBuilder.addStmt(IR.SMem(
    IR.Identifier(memImplName),
    ModuleOps.irTypeOf(dataProto),
    depth,
    readUnderWrite
  ))

  private def portWireVec[T <: HWData](cnt: Int, x: T, name: String): Vec[T] =
    ModuleOps.wire(Vec.fill(cnt)(x), Some(name), m)

  private val readEnVec = portWireVec(readPortCount, Bool(), s"${memName}_r_en")
  private val readAddrVec = portWireVec(readPortCount, UInt(Width(addrWidth)), s"${memName}_r_addr")
  private val readDataVec = portWireVec(readPortCount, HWAggregate.cloneData(dataProto), s"${memName}_r_data")

  private val writeEnVec = portWireVec(writePortCount, Bool(), s"${memName}_w_en")
  private val writeAddrVec = portWireVec(writePortCount, UInt(addrWidth.W), s"${memName}_w_addr")
  private val writeDataVec = portWireVec(writePortCount, HWAggregate.cloneData(dataProto), s"${memName}_w_data")
  private val writeMaskVec = if masked then Some(portWireVec(writePortCount, Vec.fill(maskWidth)(Bool()), s"${memName}_w_mask")) else None

  private val readWriteEnVec = portWireVec(readwritePortCount, Bool(), s"${memName}_rw_en")
  private val readWriteAddrVec = portWireVec(readwritePortCount, UInt(Width(addrWidth)), s"${memName}_rw_addr")
  private val readWriteModeVec = portWireVec(readwritePortCount, Bool(), s"${memName}_rw_wmode")
  private val readWriteWDataVec = portWireVec(readwritePortCount, HWAggregate.cloneData(dataProto), s"${memName}_rw_wdata")
  private val readWriteRDataVec = portWireVec(readwritePortCount, HWAggregate.cloneData(dataProto), s"${memName}_rw_rdata")
  private val readWriteMaskVec = if masked then Some(portWireVec(readwritePortCount, Vec.fill(maskWidth)(Bool()), s"${memName}_rw_mask")) else None

  private def connectZero(value: HWData): Unit =
    HWAggregate.foreach(value) { (leaf, _) =>
      leaf match
        case u: UInt =>
          val w = u.getWidth match
            case kw: KnownWidth => kw
            case _ => Width(1)
          ModuleOps.connect(u, 0.U(w), m)
        case b: Bool =>
          ModuleOps.connect(b, false.B, m)
        case _ => ()
    }

  (0 until readPortCount).foreach { idx =>
    ModuleOps.connect(readEnVec(idx), false.B, m)
    ModuleOps.connect(readAddrVec(idx), 0.U(Width(addrWidth)), m)
  }

  (0 until writePortCount).foreach { idx =>
    ModuleOps.connect(writeEnVec(idx), false.B, m)
    ModuleOps.connect(writeAddrVec(idx), 0.U(Width(addrWidth)), m)
    connectZero(writeDataVec(idx))
    writeMaskVec.foreach { masks =>
      val maskVec = masks(idx)
      var i = 0
      while i < maskVec.length do
        maskVec(i) := false.B
        i += 1
    }
  }

  (0 until readwritePortCount).foreach { idx =>
    ModuleOps.connect(readWriteEnVec(idx), false.B, m)
    ModuleOps.connect(readWriteAddrVec(idx), 0.U(Width(addrWidth)), m)
    ModuleOps.connect(readWriteModeVec(idx), false.B, m)
    connectZero(readWriteWDataVec(idx))
    connectZero(readWriteRDataVec(idx))
    readWriteMaskVec.foreach { masks =>
      val maskVec = masks(idx)
      var i = 0
      while i < maskVec.length do
        maskVec(i) := false.B
        i += 1
    }
  }

  private val clock = m.getImplicitClock

  private def rebindData(expr: IR.Expr): T =
    HWAggregate.rebind(HWAggregate.cloneData(dataProto), expr).asInstanceOf[T]

  private def connectMaskedVec(dst: Vec[?], src: Vec[?], mask: Vec[Bool]): Unit =
    var i = 0
    while i < mask.length do
      ModuleOps.when(mask(i), m) {
        ModuleOps.connect(dst.elems(i).asInstanceOf[HWData], src.elems(i).asInstanceOf[HWData], m)
      }
      i += 1

  (0 until readPortCount).foreach { idx =>
    val mportName = m.getBuilder.allocateName(
      Some(s"${memName}_out_r${idx}_data_MPORT"), s"${memName}_r_mport"
    )
    ModuleOps.when(readEnVec(idx), m) {
      m.getBuilder.addStmt(IR.MemPort(
        IR.Identifier(mportName),
        IR.Identifier(memImplName),
        ModuleOps.exprFor(readAddrVec(idx), m),
        ModuleOps.exprFor(clock, m),
        IR.MemPortDir.Read
      ))
    }
    val refData = rebindData(IR.Ref(IR.Identifier(mportName)))
    ModuleOps.connect(readDataVec(idx), refData, m)
  }

  (0 until writePortCount).foreach { idx =>
    val mportName = m.getBuilder.allocateName(
      Some(s"${memName}_w${idx}_MPORT"), s"${memName}_w_mport"
    )
    ModuleOps.when(writeEnVec(idx), m) {
      m.getBuilder.addStmt(IR.MemPort(
        IR.Identifier(mportName),
        IR.Identifier(memImplName),
        ModuleOps.exprFor(writeAddrVec(idx), m),
        ModuleOps.exprFor(clock, m),
        IR.MemPortDir.Write
      ))
      val mportData = rebindData(IR.Ref(IR.Identifier(mportName)))
      writeMaskVec match
        case Some(maskVec) =>
          connectMaskedVec(
            mportData.asInstanceOf[Vec[?]],
            writeDataVec(idx).asInstanceOf[Vec[?]], maskVec(idx))
        case None =>
          ModuleOps.connect(mportData, writeDataVec(idx), m)
    }
  }

  (0 until readwritePortCount).foreach { idx =>
    val mportName = m.getBuilder.allocateName(
      Some(s"${memName}_rw${idx}_MPORT"), s"${memName}_rw_mport"
    )
    ModuleOps.when(readWriteEnVec(idx), m) {
      m.getBuilder.addStmt(IR.MemPort(
        IR.Identifier(mportName),
        IR.Identifier(memImplName),
        ModuleOps.exprFor(readWriteAddrVec(idx), m),
        ModuleOps.exprFor(clock, m),
        IR.MemPortDir.ReadWrite
      ))
      val mportData = rebindData(IR.Ref(IR.Identifier(mportName)))
      ModuleOps.when(readWriteModeVec(idx), m) {
        readWriteMaskVec match
          case Some(maskVec) =>
            connectMaskedVec(
              mportData.asInstanceOf[Vec[?]],
              readWriteWDataVec(idx).asInstanceOf[Vec[?]], maskVec(idx))
          case None =>
            ModuleOps.connect(mportData, readWriteWDataVec(idx), m)
      }
    }
    val refData = rebindData(IR.Ref(IR.Identifier(mportName)))
    ModuleOps.connect(readWriteRDataVec(idx), refData, m)
  }

  val readPorts  =     SRAMReadPorts(readEnVec,
                                     readAddrVec,
                                     readDataVec)

  val writePorts =     SRAMWritePorts(writeEnVec,
                                      writeAddrVec,
                                      writeDataVec,
                                      writeMaskVec)

  val readwritePorts = SRAMReadWritePorts(readWriteEnVec,
                                          readWriteAddrVec,
                                          readWriteModeVec,
                                          readWriteWDataVec,
                                          readWriteRDataVec,
                                          readWriteMaskVec)

object SRAM:
  def apply[T <: HWData](
    data: T, depth: Int
  )(
    reads: Int, writes: Int, readwrites: Int, masked: Boolean = false
  )(using Module): SRAM[T] =
    new SRAM(data, depth, reads, writes, readwrites, masked = masked)

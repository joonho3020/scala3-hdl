package hdl

final class SRAMReadPortHandle[T <: HWData](val enable: Bool, val address: UInt, val data: T)(using Module):
  def read(addr: UInt): T =
    enable := true.B
    address := addr
    data

  def read(addr: UInt, en: Bool): T =
    enable := en
    address := addr
    data

final class SRAMWritePortHandle[T <: HWData](val enable: Bool, val address: UInt, val data: T)(using Module):
  def write(addr: UInt, payload: T): Unit =
    enable := true.B
    address := addr
    data := payload

  def write(addr: UInt, payload: T, en: Bool): Unit =
    enable := en
    address := addr
    data := payload

final class SRAMReadWritePortHandle[T <: HWData](
  val enable: Bool,
  val address: UInt,
  val isWrite: Bool,
  val writeData: T,
  val readData: T
)(using Module):
  def write(addr: UInt, payload: T): Unit =
    enable := true.B
    isWrite := true.B
    address := addr
    writeData := payload

  def write(addr: UInt, payload: T, en: Bool): Unit =
    enable := en
    isWrite := true.B
    address := addr
    writeData := payload

  def read(addr: UInt): T =
    enable := true.B
    isWrite := false.B
    address := addr
    readData

  def read(addr: UInt, en: Bool): T =
    enable := en
    isWrite := false.B
    address := addr
    readData

final class SRAMReadPorts[T <: HWData](en: Vec[Bool], addr: Vec[UInt], data: Vec[T])(using Module):
  def apply(idx: Int): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

  def apply(idx: UInt): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

final class SRAMWritePorts[T <: HWData](en: Vec[Bool], addr: Vec[UInt], data: Vec[T])(using Module):
  def apply(idx: Int): SRAMWritePortHandle[T] =
    SRAMWritePortHandle(en(idx), addr(idx), data(idx))

  def apply(idx: UInt): SRAMWritePortHandle[T] =
    SRAMWritePortHandle(en(idx), addr(idx), data(idx))

final class SRAMReadWritePorts[T <: HWData](
  en: Vec[Bool],
  addr: Vec[UInt],
  wmode: Vec[Bool],
  wdata: Vec[T],
  rdata: Vec[T]
)(using Module):
  def apply(idx: Int): SRAMReadWritePortHandle[T] =
    SRAMReadWritePortHandle(en(idx), addr(idx), wmode(idx), wdata(idx), rdata(idx))

  def apply(idx: UInt): SRAMReadWritePortHandle[T] =
    SRAMReadWritePortHandle(en(idx), addr(idx), wmode(idx), wdata(idx), rdata(idx))

final class SRAM[T <: HWData](
  data: T,
  depth: Int,
  val readPortCount: Int,
  val writePortCount: Int,
  val readwritePortCount: Int,
  val readLatency: Int = 1,
  val writeLatency: Int = 1,
  val readUnderWrite: IR.ReadUnderWrite = IR.ReadUnderWrite.Undefined
)(using m: Module):
  val addrWidth = log2Ceil(depth)

  private val dataProto = HWAggregate.cloneData(data)
  private val memName = m.getBuilder.allocateName(Some("mem"), "mem")

  private val readerIds = (0 until readPortCount).map(i => IR.Identifier(s"r$i"))
  private val writerIds = (0 until writePortCount).map(i => IR.Identifier(s"w$i"))
  private val readwriterIds = (0 until readwritePortCount).map(i => IR.Identifier(s"rw$i"))

  m.getBuilder.addStmt(IR.Mem(
    IR.Identifier(memName),
    ModuleOps.irTypeOf(dataProto),
    depth,
    readLatency,
    writeLatency,
    readerIds,
    writerIds,
    readwriterIds,
    readUnderWrite
  ))

  private val readEnVec = ModuleOps.wire(Vec.fill(readPortCount)(Bool()), Some(s"${memName}_r_en"), m)
  private val readAddrVec = ModuleOps.wire(Vec.fill(readPortCount)(UInt(Width(addrWidth))), Some(s"${memName}_r_addr"), m)
  private val readDataVec = ModuleOps.wire(Vec.fill(readPortCount)(HWAggregate.cloneData(dataProto)), Some(s"${memName}_r_data"), m)

  private val writeEnVec = ModuleOps.wire(Vec.fill(writePortCount)(Bool()), Some(s"${memName}_w_en"), m)
  private val writeAddrVec = ModuleOps.wire(Vec.fill(writePortCount)(UInt(Width(addrWidth))), Some(s"${memName}_w_addr"), m)
  private val writeDataVec = ModuleOps.wire(Vec.fill(writePortCount)(HWAggregate.cloneData(dataProto)), Some(s"${memName}_w_data"), m)

  private val readWriteEnVec = ModuleOps.wire(Vec.fill(readwritePortCount)(Bool()), Some(s"${memName}_rw_en"), m)
  private val readWriteAddrVec = ModuleOps.wire(Vec.fill(readwritePortCount)(UInt(Width(addrWidth))), Some(s"${memName}_rw_addr"), m)
  private val readWriteModeVec = ModuleOps.wire(Vec.fill(readwritePortCount)(Bool()), Some(s"${memName}_rw_wmode"), m)
  private val readWriteWDataVec = ModuleOps.wire(Vec.fill(readwritePortCount)(HWAggregate.cloneData(dataProto)), Some(s"${memName}_rw_wdata"), m)
  private val readWriteRDataVec = ModuleOps.wire(Vec.fill(readwritePortCount)(HWAggregate.cloneData(dataProto)), Some(s"${memName}_rw_rdata"), m)

  private val oneMask = 1.U(Width(1))

  private def portExpr(port: IR.Identifier): IR.Expr =
    IR.SubField(IR.Ref(IR.Identifier(memName)), port)

  private def fieldExpr(port: IR.Identifier, field: String): IR.Expr =
    IR.SubField(portExpr(port), IR.Identifier(field))

  private def rebindBool(expr: IR.Expr): Bool =
    HWAggregate.rebind(Bool(), expr)

  private def rebindAddr(expr: IR.Expr): UInt =
    HWAggregate.rebind(UInt(Width(addrWidth)), expr)

  private def rebindMask(expr: IR.Expr): UInt =
    HWAggregate.rebind(UInt(Width(1)), expr)

  private def rebindData(expr: IR.Expr): T =
    HWAggregate.rebind(HWAggregate.cloneData(dataProto), expr).asInstanceOf[T]

  readerIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val dataField = rebindData(fieldExpr(pid, "data"))
    ModuleOps.connect(en, readEnVec.elems(idx), m)
    ModuleOps.connect(addr, readAddrVec.elems(idx), m)
    ModuleOps.connect(readDataVec.elems(idx), dataField, m)
  }

  writerIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val dataField = rebindData(fieldExpr(pid, "data"))
    val maskField = rebindMask(fieldExpr(pid, "mask"))
    ModuleOps.connect(en, writeEnVec.elems(idx), m)
    ModuleOps.connect(addr, writeAddrVec.elems(idx), m)
    ModuleOps.connect(dataField, writeDataVec.elems(idx), m)
    ModuleOps.connect(maskField, oneMask, m)
  }

  readwriterIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val wmode = rebindBool(fieldExpr(pid, "wmode"))
    val wdata = rebindData(fieldExpr(pid, "wdata"))
    val rdata = rebindData(fieldExpr(pid, "rdata"))
    val mask = rebindMask(fieldExpr(pid, "wmask"))
    ModuleOps.connect(en, readWriteEnVec.elems(idx), m)
    ModuleOps.connect(addr, readWriteAddrVec.elems(idx), m)
    ModuleOps.connect(wmode, readWriteModeVec.elems(idx), m)
    ModuleOps.connect(wdata, readWriteWDataVec.elems(idx), m)
    ModuleOps.connect(readWriteRDataVec.elems(idx), rdata, m)
    ModuleOps.connect(mask, oneMask, m)
  }

  val readPorts = SRAMReadPorts(readEnVec, readAddrVec, readDataVec)
  val writePorts = SRAMWritePorts(writeEnVec, writeAddrVec, writeDataVec)
  val readwritePorts = SRAMReadWritePorts(readWriteEnVec, readWriteAddrVec, readWriteModeVec, readWriteWDataVec, readWriteRDataVec)

object SRAM:
  def apply[T <: HWData](data: T, depth: Int)(reads: Int, writes: Int, readwrites: Int)(using Module): SRAM[T] =
    new SRAM(data, depth, reads, writes, readwrites)

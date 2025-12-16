package hdl

private def connectMaskField(dst: HWData, src: HWData)(using Module): Unit =
  (dst, src) match
    case (dv: Vec[?], sv: Vec[?]) =>
      val len = math.min(dv.length, sv.length)
      var i = 0
      while i < len do
        connectMaskField(dv.elems(i).asInstanceOf[HWData], sv.elems(i).asInstanceOf[HWData])
        i += 1
    case (dv: Vec[?], s) =>
      var i = 0
      while i < dv.length do
        connectMaskField(dv.elems(i).asInstanceOf[HWData], s)
        i += 1
    case (db: Bundle[?], sb: Bundle[?]) =>
      val dp = db.asInstanceOf[Product]
      val sp = sb.asInstanceOf[Product]
      val arity = math.min(dp.productArity, sp.productArity)
      var i = 0
      while i < arity do
        connectMaskField(dp.productElement(i).asInstanceOf[HWData], sp.productElement(i).asInstanceOf[HWData])
        i += 1
    case (db: Bundle[?], su: UInt) =>
      HWAggregate.foreach(db) { (leaf, _) =>
        leaf match
          case u: UInt => ModuleOps.connect(u, su, summon[Module])
          case b: Bool => ModuleOps.connect(b, su.asUInt(using summon[Module]), summon[Module])
          case _ => ()
      }
    case (db: Bundle[?], sb: Bool) =>
      HWAggregate.foreach(db) { (leaf, _) =>
        leaf match
          case u: UInt => ModuleOps.connect(u, sb.asUInt(using summon[Module]), summon[Module])
          case b: Bool => ModuleOps.connect(b, sb, summon[Module])
          case _ => ()
      }
    case (du: UInt, su: UInt) =>
      ModuleOps.connect(du, su, summon[Module])
    case (db: Bool, sb: Bool) =>
      ModuleOps.connect(db, sb, summon[Module])
    case (db: Bool, su: UInt) =>
      ModuleOps.connect(db, su.asBool(using summon[Module]), summon[Module])
    case _ => ()

final class SRAMReadPortHandle[T <: HWData](val enable: Bool, val address: UInt, val data: T)(using Module):
  def read(addr: UInt): T =
    enable := true.B
    address := addr
    data

  def read(addr: UInt, en: Bool): T =
    enable := en
    address := addr
    data

final class SRAMWritePortHandle[T <: HWData, M <: HWData](val enable: Bool, val address: UInt, val data: T, val mask: M)(using Module):
  private def fillMask(bit: Boolean): Unit =
    val lit: HWData = (if bit then 1.U(Width(1)) else 0.U(Width(1))).asInstanceOf[HWData]
    connectMaskField(mask.asInstanceOf[HWData], lit)

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

  def write(addr: UInt, payload: T, maskValue: M): Unit =
    enable := true.B
    address := addr
    data := payload
    connectMaskField(mask.asInstanceOf[HWData], maskValue.asInstanceOf[HWData])

final class SRAMReadWritePortHandle[T <: HWData, M <: HWData](
  val enable: Bool,
  val address: UInt,
  val isWrite: Bool,
  val writeData: T,
  val readData: T,
  val mask: M
)(using Module):
  private def fillMask(bit: Boolean): Unit =
    val lit: HWData = (if bit then 1.U(Width(1)) else 0.U(Width(1))).asInstanceOf[HWData]
    connectMaskField(mask.asInstanceOf[HWData], lit)

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

  def write(addr: UInt, payload: T, maskValue: M): Unit =
    enable := true.B
    isWrite := true.B
    address := addr
    writeData := payload
    connectMaskField(mask.asInstanceOf[HWData], maskValue.asInstanceOf[HWData])

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

final class SRAMReadPorts[T <: HWData](en: Vec[Bool], addr: Vec[UInt], data: Vec[T])(using Module):
  def apply(idx: Int): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

  def apply(idx: UInt): SRAMReadPortHandle[T] =
    SRAMReadPortHandle(en(idx), addr(idx), data(idx))

final class SRAMWritePorts[T <: HWData, M <: HWData](en: Vec[Bool], addr: Vec[UInt], data: Vec[T], mask: Vec[M])(using Module):
  def apply(idx: Int): SRAMWritePortHandle[T, M] =
    SRAMWritePortHandle(en(idx), addr(idx), data(idx), mask(idx))

  def apply(idx: UInt): SRAMWritePortHandle[T, M] =
    SRAMWritePortHandle(en(idx), addr(idx), data(idx), mask(idx))

final class SRAMReadWritePorts[T <: HWData, M <: HWData](
  en: Vec[Bool],
  addr: Vec[UInt],
  wmode: Vec[Bool],
  wdata: Vec[T],
  rdata: Vec[T],
  mask: Vec[M]
)(using Module):
  def apply(idx: Int): SRAMReadWritePortHandle[T, M] =
    SRAMReadWritePortHandle(en(idx), addr(idx), wmode(idx), wdata(idx), rdata(idx), mask(idx))

  def apply(idx: UInt): SRAMReadWritePortHandle[T, M] =
    SRAMReadWritePortHandle(en(idx), addr(idx), wmode(idx), wdata(idx), rdata(idx), mask(idx))

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

  private def rebuildProductLike(p: Product, values: Array[Any]): Any =
    val cls = p.getClass
    val ctors = cls.getConstructors
    val args = values.map(_.asInstanceOf[AnyRef])
    val direct = ctors.collectFirst {
      case c if c.getParameterCount == args.length =>
        () => c.newInstance(args*)
    }
    val outer = ctors.collectFirst {
      case c if c.getParameterCount == args.length + 1 =>
        cls.getDeclaredFields.find(_.getName == "$outer").map { outerField =>
          outerField.setAccessible(true)
          val outer = outerField.get(p)
          val outerArgs: Array[AnyRef] = Array(outer.asInstanceOf[AnyRef]) ++ args
          () => c.newInstance(outerArgs*)
        }
    }.flatten
    direct.orElse(outer)
      .map(_())
      .getOrElse {
        val companionOpt = try Some(Class.forName(s"${cls.getName}$$")) catch
          case _: Throwable => None
        val module = companionOpt.map(_.getField("MODULE$").get(null))
          .getOrElse(throw new IllegalArgumentException(s"No apply method for ${cls.getName}"))
        val apply = module.getClass.getMethods.find(m => m.getName == "apply" && m.getParameterCount == args.length)
          .getOrElse(throw new IllegalArgumentException(s"No apply method for ${cls.getName}"))
        apply.invoke(module, args*)
      }

  private def maskProto(d: HWData): HWData = d match
    case v: Vec[?] =>
      val elems = v.elems.map(e => maskProto(e.asInstanceOf[HWData]))
      Vec(elems.asInstanceOf[Seq[HWData]])
    case b: Bundle[?] =>
      val prod = b.asInstanceOf[Product]
      val arity = prod.productArity
      val values = Array.ofDim[Any](arity)
      var i = 0
      while i < arity do
        values(i) = maskProto(prod.productElement(i).asInstanceOf[HWData])
        i += 1
      rebuildProductLike(prod, values).asInstanceOf[HWData]
    case _: Bool => Bool()
    case _ => UInt(Width(1))

  private val maskTemplate = maskProto(dataProto)

  private val writeMaskVec = ModuleOps.wire(Vec.fill(writePortCount)(HWAggregate.cloneData(maskTemplate)), Some(s"${memName}_w_mask"), m)
  private val readWriteMaskVec = ModuleOps.wire(Vec.fill(readwritePortCount)(HWAggregate.cloneData(maskTemplate)), Some(s"${memName}_rw_mask"), m)

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
    ModuleOps.connect(readEnVec.elems(idx), false.B, m)
    ModuleOps.connect(readAddrVec.elems(idx), 0.U(Width(addrWidth)), m)
  }

  (0 until writePortCount).foreach { idx =>
    ModuleOps.connect(writeEnVec.elems(idx), false.B, m)
    ModuleOps.connect(writeAddrVec.elems(idx), 0.U(Width(addrWidth)), m)
    connectZero(writeDataVec.elems(idx))
    connectZero(writeMaskVec.elems(idx))
  }

  (0 until readwritePortCount).foreach { idx =>
    ModuleOps.connect(readWriteEnVec.elems(idx), false.B, m)
    ModuleOps.connect(readWriteAddrVec.elems(idx), 0.U(Width(addrWidth)), m)
    ModuleOps.connect(readWriteModeVec.elems(idx), false.B, m)
    connectZero(readWriteWDataVec.elems(idx))
    connectZero(readWriteMaskVec.elems(idx))
  }

  private val clock = m.getImplicitClock

  private def portExpr(port: IR.Identifier): IR.Expr =
    IR.SubField(IR.Ref(IR.Identifier(memName)), port)

  private def fieldExpr(port: IR.Identifier, field: String): IR.Expr =
    IR.SubField(portExpr(port), IR.Identifier(field))

  private def rebindBool(expr: IR.Expr): Bool =
    HWAggregate.rebind(Bool(), expr)

  private def rebindAddr(expr: IR.Expr): UInt =
    HWAggregate.rebind(UInt(Width(addrWidth)), expr)

  private def rebindMask(expr: IR.Expr): HWData =
    HWAggregate.rebind(maskProto(dataProto), expr)

  private def rebindData(expr: IR.Expr): T =
    HWAggregate.rebind(HWAggregate.cloneData(dataProto), expr).asInstanceOf[T]

  private def rebindClock(expr: IR.Expr): Clock =
    HWAggregate.rebind(Clock(), expr)

  readerIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val dataField = rebindData(fieldExpr(pid, "data"))
    val clk = rebindClock(fieldExpr(pid, "clk"))
    ModuleOps.connect(en, readEnVec.elems(idx), m)
    ModuleOps.connect(addr, readAddrVec.elems(idx), m)
    ModuleOps.connect(readDataVec.elems(idx), dataField, m)
    ModuleOps.connect(clk, clock, m)
  }

  writerIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val dataField = rebindData(fieldExpr(pid, "data"))
    val maskField = rebindMask(fieldExpr(pid, "mask"))
    val clk = rebindClock(fieldExpr(pid, "clk"))
    ModuleOps.connect(en, writeEnVec.elems(idx), m)
    ModuleOps.connect(addr, writeAddrVec.elems(idx), m)
    ModuleOps.connect(dataField, writeDataVec.elems(idx), m)
    connectMaskField(maskField, writeMaskVec.elems(idx))
    ModuleOps.connect(clk, clock, m)
  }

  readwriterIds.zipWithIndex.foreach { case (pid, idx) =>
    val en = rebindBool(fieldExpr(pid, "en"))
    val addr = rebindAddr(fieldExpr(pid, "addr"))
    val wmode = rebindBool(fieldExpr(pid, "wmode"))
    val wdata = rebindData(fieldExpr(pid, "wdata"))
    val rdata = rebindData(fieldExpr(pid, "rdata"))
    val mask = rebindMask(fieldExpr(pid, "wmask"))
    val clk = rebindClock(fieldExpr(pid, "clk"))
    ModuleOps.connect(en, readWriteEnVec.elems(idx), m)
    ModuleOps.connect(addr, readWriteAddrVec.elems(idx), m)
    ModuleOps.connect(wmode, readWriteModeVec.elems(idx), m)
    ModuleOps.connect(wdata, readWriteWDataVec.elems(idx), m)
    ModuleOps.connect(readWriteRDataVec.elems(idx), rdata, m)
    connectMaskField(mask, readWriteMaskVec.elems(idx))
    ModuleOps.connect(clk, clock, m)
  }

  val readPorts = SRAMReadPorts(readEnVec, readAddrVec, readDataVec)
  val writePorts = SRAMWritePorts(writeEnVec, writeAddrVec, writeDataVec, writeMaskVec)
  val readwritePorts = SRAMReadWritePorts(readWriteEnVec, readWriteAddrVec, readWriteModeVec, readWriteWDataVec, readWriteRDataVec, readWriteMaskVec)

object SRAM:
  def apply[T <: HWData](data: T, depth: Int)(reads: Int, writes: Int, readwrites: Int)(using Module): SRAM[T] =
    new SRAM(data, depth, reads, writes, readwrites)

package hdl

final case class Connection(dst: Node[?], src: Node[?])

final case class ElaboratedDesign(nodes: Seq[Node[?]], connections: Seq[Connection])

final class ModuleBuilder:
  private val nodes = collection.mutable.ArrayBuffer.empty[Node[?]]
  private val connections = collection.mutable.ArrayBuffer.empty[Connection]
  def register[T <: ValueType](n: Node[T]): Node[T] =
    nodes += n
    n
  def connect(dst: Node[?], src: Node[?]): Unit =
    connections += Connection(dst, src)
  def snapshot: ElaboratedDesign =
    ElaboratedDesign(nodes.toVector, connections.toVector)

abstract class Module:
  private val builder = new ModuleBuilder
  protected given ModuleBuilder = builder
  private lazy val built: ElaboratedDesign =
    builder.snapshot
  final def design: ElaboratedDesign = built
  protected def IO[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.IO(t, name))
  protected def Wire[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.Wire(t, name))
  protected def Reg[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.Reg(t, name))
  protected def PrimOp[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.PrimOp(t, name))

extension [T <: ValueType](dst: Node[T])
  def :=(src: Node[T])(using b: ModuleBuilder): Unit =
    b.connect(dst, src)

extension (lhs: Node[UInt])
  def +(rhs: Node[UInt])(using b: ModuleBuilder): Node[UInt] =
    val out = b.register(hdl.PrimOp(UInt(lhs.tpe.w)))
    b.connect(out, lhs)
    b.connect(out, rhs)
    out

final class Elaborator:
  def elaborate(top: Module): ElaboratedDesign = top.design
  def emit(design: ElaboratedDesign): String =
    val ids = new java.util.IdentityHashMap[Node[?], String]()
    design.nodes.zipWithIndex.foreach { case (n, i) => ids.put(n, s"n$i") }
    val nodesStr = design.nodes.zipWithIndex.map { case (n, i) =>
      val id = s"n$i"
      val nameStr = n.name.fold("")(s => s":$s")
      s"$id ${n.kind} ${n.tpe}$nameStr"
    }
    val connsStr = design.connections.map { c =>
      val dstId = Option(ids.get(c.dst)).getOrElse("?")
      val srcId = Option(ids.get(c.src)).getOrElse("?")
      s"$dstId <= $srcId"
    }
    (nodesStr ++ connsStr).mkString("\n")

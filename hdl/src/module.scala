package hdl

final case class Connection(dst: Node[?], src: Node[?])

final case class ElaboratedDesign(nodes: Seq[Node[?]], connections: Seq[Connection])

final class ModuleBuilder:
  private val nodes = collection.mutable.ArrayBuffer.empty[Node[?]]
  private val connections = collection.mutable.ArrayBuffer.empty[Connection]
  private val names = collection.mutable.Map.empty[Node[?], String]
  def register[T <: ValueType](n: Node[T]): Node[T] =
    nodes += n
    n.name.foreach(names.update(n, _))
    n
  def connect(dst: Node[?], src: Node[?]): Unit =
    connections += Connection(dst, src)
  def nameOf(n: Node[?], fallback: String): String =
    names.getOrElse(n, fallback)
  def snapshot: ElaboratedDesign =
    ElaboratedDesign(nodes.toVector, connections.toVector)

abstract class Module(using protected val builder: ModuleBuilder = new ModuleBuilder):
  final def design: ElaboratedDesign = builder.snapshot
  protected def IO[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.IO(t, name))
  protected def Wire[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.Wire(t, name))
  protected def Reg[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.Reg(t, name))
  protected def PrimOp[T <: ValueType](t: T, name: Option[String] = None): Node[T] =
    builder.register(hdl.PrimOp(t, name))

object Module:
  def apply[M <: Module](gen: => M)(using b: ModuleBuilder): M = gen

extension [T <: ValueType](dst: Node[T])
  def :=(src: Node[T])(using b: ModuleBuilder): Unit =
    b.connect(dst, src)

extension (lhs: Node[UInt])
  def +(rhs: Node[UInt])(using b: ModuleBuilder): Node[UInt] =
    val out = b.register(hdl.PrimOp(UInt(lhs.tpe.w), Some("+")))
    b.connect(out, lhs)
    b.connect(out, rhs)
    out

final class Elaborator:
  def elaborate(top: Module): ElaboratedDesign = top.design
  def emit(design: ElaboratedDesign): String =
    val ids = new java.util.IdentityHashMap[Node[?], String]()
    design.nodes.zipWithIndex.foreach { case (n, i) => ids.put(n, s"n$i") }
    def fmt(n: Node[?]): String =
      n.name.orElse(Option(ids.get(n))).getOrElse(s"${n.kind}")
    val decls = design.nodes.zipWithIndex.map { case (n, i) =>
      val nm = fmt(n)
      val kindStr = n.kind match
        case NodeKind.IO     => "io"
        case NodeKind.Reg    => "reg"
        case NodeKind.Wire   => "wire"
        case NodeKind.PrimOp => "op"
        case NodeKind.Lit    => "lit"
      s"$kindStr $nm: ${n.tpe}"
    }
    val opStmts = design.nodes.filter(_.kind == NodeKind.PrimOp).map { op =>
      val inputs = design.connections.filter(_.dst eq op).map(_.src)
      val lhs = fmt(op)
      val opSym = op.name.getOrElse("+")
      val rhs = inputs.map(fmt).mkString(s" $opSym ")
      s"$lhs = $rhs"
    }
    val assigns = design.connections.filter(_.dst.kind != NodeKind.PrimOp).map { c =>
      val dstId = fmt(c.dst)
      val srcId = fmt(c.src)
      s"$dstId := $srcId"
    }
    (decls ++ opStmts ++ assigns).mkString("\n")

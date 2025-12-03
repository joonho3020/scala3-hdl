package hdl

import scala.quoted.*

final case class ElaboratedDesign(name: String, ports: Seq[String], body: Seq[String])

final class ModuleBuilder(val moduleName: String):
  private val ports = collection.mutable.ArrayBuffer.empty[String]
  private val body = collection.mutable.ArrayBuffer.empty[String]
  private var tempCounter = 0

  def freshName(prefix: String): String =
    tempCounter += 1
    s"${prefix}_$tempCounter"

  def addPort(stmt: String): Unit = ports += stmt
  def addBody(stmt: String): Unit = body += stmt

  def snapshot: ElaboratedDesign =
    ElaboratedDesign(moduleName, ports.toVector, body.toVector)

abstract class Module:
  private[hdl] val _builder: ModuleBuilder = new ModuleBuilder(moduleName)

  def moduleName: String = getClass.getSimpleName.stripSuffix("$")

  private[hdl] def getBuilder: ModuleBuilder = _builder

  protected inline def IO[T <: ValueType](inline t: T): Node[T] =
    ${ Macros.ioImpl('t, 'this) }

  protected inline def Wire[T <: ValueType](inline t: T): Node[T] =
    ${ Macros.wireImpl('t, 'this) }

  protected inline def Reg[T <: ValueType](inline t: T): Node[T] =
    ${ Macros.regImpl('t, 'this) }

  final def design: ElaboratedDesign = _builder.snapshot

object Module:
  inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
    ${ Macros.moduleInstImpl('gen, 'parent) }

extension [T <: ValueType](dst: Node[T])
  def :=(src: Node[?])(using m: Module): Unit =
    m.getBuilder.addBody(s"connect ${dst.ref}, ${src.ref}")

extension [T <: ValueType](lhs: Node[T])
  def +(rhs: Node[?])(using m: Module): Node[T] =
    val tmpName = m.getBuilder.freshName("add")
    m.getBuilder.addBody(s"node $tmpName = add(${lhs.ref}, ${rhs.ref})")
    Node(lhs.tpe, NodeKind.PrimOp, Some(tmpName), None, tmpName)

final class Elaborator:
  private val elaborated = collection.mutable.Map.empty[String, ElaboratedDesign]

  def elaborate(top: Module): ElaboratedDesign =
    elaborateModule(top)
    elaborated(top.moduleName)

  private[hdl] def elaborateModule(mod: Module): Unit =
    if elaborated.contains(mod.moduleName) then return
    discoverSubmodules(mod)
    elaborated(mod.moduleName) = mod.design

  private def discoverSubmodules(mod: Module): Unit =
    val clazz = mod.getClass
    for field <- clazz.getDeclaredFields do
      field.setAccessible(true)
      field.get(mod) match
        case submod: Module if submod ne mod =>
          elaborateModule(submod)
        case _ => ()

  def emit(design: ElaboratedDesign): String =
    val sb = new StringBuilder
    sb.append(s"module ${design.name}:\n")
    design.ports.foreach(p => sb.append(s"  $p\n"))
    sb.append("\n")
    design.body.foreach(s => sb.append(s"  $s\n"))
    sb.toString

  def emitAll: String =
    elaborated.values.map(emit).mkString("\n")

object Macros:
  def findEnclosingValName(using q: Quotes): String =
    import q.reflect.*
    def searchOwners(sym: Symbol): String =
      if sym.isNoSymbol then "unnamed"
      else if sym.isValDef then sym.name
      else searchOwners(sym.owner)
    searchOwners(Symbol.spliceOwner.owner)

  def ioImpl[T <: ValueType: Type](
    t: Expr[T],
    mod: Expr[Module]
  )(using q: Quotes): Expr[Node[T]] =
    import q.reflect.*
    val name = findEnclosingValName
    '{
      val tpe = $t
      val m = $mod
      val portDecls = emitPortDecl(${Expr(name)}, tpe)
      portDecls.foreach(m.getBuilder.addPort)
      Node(tpe, NodeKind.IO, Some(${Expr(name)}), None, ${Expr(name)})
    }

  def wireImpl[T <: ValueType: Type](
    t: Expr[T],
    mod: Expr[Module]
  )(using q: Quotes): Expr[Node[T]] =
    import q.reflect.*
    val name = findEnclosingValName
    '{
      val tpe = $t
      val m = $mod
      m.getBuilder.addBody(s"wire ${${Expr(name)}} : ${formatType(tpe)}")
      Node(tpe, NodeKind.Wire, Some(${Expr(name)}), None, ${Expr(name)})
    }

  def regImpl[T <: ValueType: Type](
    t: Expr[T],
    mod: Expr[Module]
  )(using q: Quotes): Expr[Node[T]] =
    import q.reflect.*
    val name = findEnclosingValName
    '{
      val tpe = $t
      val m = $mod
      m.getBuilder.addBody(s"reg ${${Expr(name)}} : ${formatType(tpe)}, clock")
      Node(tpe, NodeKind.Reg, Some(${Expr(name)}), None, ${Expr(name)})
    }

  def moduleInstImpl[M <: Module: Type](
    gen: Expr[M],
    parent: Expr[Module]
  )(using q: Quotes): Expr[M] =
    import q.reflect.*
    val name = findEnclosingValName
    '{
      val submod = $gen
      val m = $parent
      m.getBuilder.addBody(s"inst ${${Expr(name)}} of ${submod.moduleName}")
      setSubmodulePrefix(submod, ${Expr(name)})
      submod
    }

def emitPortDecl(name: String, tpe: ValueType, prefix: String = ""): Seq[String] =
  val fullName = if prefix.isEmpty then name else s"$prefix.$name"
  tpe match
    case u: UInt =>
      val dirStr = if u.dir == Direction.In then "input" else "output"
      Seq(s"$dirStr $fullName : UInt<${u.w.value}>")
    case b: Bool =>
      val dirStr = if b.dir == Direction.In then "input" else "output"
      Seq(s"$dirStr $fullName : Bool")
    case bundle: Bundle =>
      val p = bundle.asInstanceOf[Product]
      (0 until p.productArity).flatMap { i =>
        val fieldName = p.productElementName(i)
        val fieldVal = p.productElement(i).asInstanceOf[ValueType]
        emitPortDecl(fieldName, fieldVal, fullName)
      }

def setSubmodulePrefix(mod: Module, prefix: String): Unit =
  val clazz = mod.getClass
  for field <- clazz.getDeclaredFields do
    field.setAccessible(true)
    field.get(mod) match
      case node: Node[?] if node.kind == NodeKind.IO =>
        node.setRef(s"$prefix.${node.ref}")
      case _ => ()

def formatType(tpe: ValueType): String = tpe match
  case u: UInt => s"UInt<${u.w.value}>"
  case b: Bool => "Bool"
  case bundle: Bundle =>
    val p = bundle.asInstanceOf[Product]
    val fields = (0 until p.productArity).map { i =>
      val fieldName = p.productElementName(i)
      val fieldVal = p.productElement(i).asInstanceOf[ValueType]
      val fieldDir = fieldVal match
        case u: UInt => if u.dir == Direction.In then "flip " else ""
        case b: Bool => if b.dir == Direction.In then "flip " else ""
        case _: Bundle => ""
      s"$fieldDir$fieldName : ${formatType(fieldVal)}"
    }
    s"{ ${fields.mkString(", ")} }"

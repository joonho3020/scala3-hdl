package hdl

import scala.quoted.*
import scala.concurrent.{Future, Await}
import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration
import scala.util.hashing.MurmurHash3
import java.security.MessageDigest
import java.util.HexFormat

// final case class ElaboratedDesign(
// name: String,
// ports: Seq[String],
// body: Seq[String]
// )

// final class ModuleBuilder(val moduleName: String):
// private val ports = collection.mutable.ArrayBuffer.empty[String]
// private val body = collection.mutable.ArrayBuffer.empty[String]
// private val usedNames = collection.mutable.Set.empty[String]
// private var tempCounter = 0

// def freshName(prefix: String): String =
// tempCounter += 1
// val candidate = s"${prefix}_$tempCounter"
// if usedNames.contains(candidate) then freshName(prefix)
// else
// usedNames += candidate
// candidate

// def allocateName(name: Option[String], prefix: String): String =
// name.filter(_.nonEmpty).flatMap { n =>
// if usedNames.contains(n) then None
// else
// usedNames += n
// Some(n)
// }.getOrElse(freshName(prefix))

// def addPort(stmt: String): Unit = ports += stmt
// def addBody(stmt: String): Unit = body += stmt

// def snapshot: ElaboratedDesign =
// ElaboratedDesign(moduleName, ports.toVector, body.toVector)

// abstract class Module:
// private[hdl] val _builder: ModuleBuilder = new ModuleBuilder(moduleName)
// private var _elabKey: Option[String] = None

// def moduleName: String = getClass.getSimpleName.stripSuffix("$")
// def elaborationKey: String = _elabKey.getOrElse(Module.autoElaborationKey(this))
// private[hdl] def setElabKey(k: String): Unit = _elabKey = Some(k)

// private[hdl] def getBuilder: ModuleBuilder = _builder

// protected inline def IO[T <: ValueType](inline t: T): Node[T] =
// ${ Macros.ioImpl('t, 'this) }

// protected inline def Wire[T <: ValueType](inline t: T): Node[T] =
// ${ Macros.wireImpl('t, 'this) }

// protected inline def Reg[T <: ValueType](inline t: T): Node[T] =
// ${ Macros.regImpl('t, 'this) }

// protected inline def Lit[T <: ValueType](inline t: T)(inline payload: HostTypeOf[T]): Node[T] =
// ${ Macros.litImpl('t, 'payload, 'this) }

// final def design: ElaboratedDesign = _builder.snapshot

// object Module:
// inline def apply[M <: hdl.Module](inline gen: M)(using inline parent: hdl.Module): M =
// ${ Macros.moduleInstImpl('gen, 'parent) }

// private def autoElaborationKey(mod: Module): String =
// s"${mod.moduleName}@${System.identityHashCode(mod)}"

// extension [T <: ValueType](dst: Node[T])
// def :=(src: Node[?])(using m: Module): Unit =
// src.kind match
// case hdl.NodeKind.Lit =>
// m.getBuilder.addBody(s"connect ${dst.ref}, ${src}")
// case _ =>
// m.getBuilder.addBody(s"connect ${dst.ref}, ${src.ref}")

// extension [T <: ValueType](lhs: Node[T])
// def +(rhs: Node[?])(using m: Module): Node[T] =
// val tmpName = m.getBuilder.freshName("add")
// m.getBuilder.addBody(s"node $tmpName = add(${lhs.ref}, ${rhs.ref})")
// Node(lhs.tpe, NodeKind.PrimOp, Some(tmpName), None, tmpName)

// final class Elaborator:
// private implicit val ec: ExecutionContext = ExecutionContext.global
// private val elaborated =
// collection.concurrent.TrieMap.empty[String, Future[ElaboratedDesign]]

// def elaborate(top: Module): ElaboratedDesign =
// Await.result(elaborateAsync(top), Duration.Inf)

// private[hdl] def elaborateAsync(mod: Module): Future[ElaboratedDesign] =
// val key = mod.elaborationKey
// elaborated.getOrElseUpdate(key, Future:
// val subs = discoverSubmodules(mod)
// val subFuts = subs.map(elaborateAsync)
// subFuts.foreach(f => Await.result(f, Duration.Inf))
// mod.design
// )

// private def discoverSubmodules(mod: Module): Seq[Module] =
// val buf = collection.mutable.ArrayBuffer.empty[Module]
// val clazz = mod.getClass
// for field <- clazz.getDeclaredFields do
// field.setAccessible(true)
// field.get(mod) match
// case submod: Module if submod ne mod =>
// buf += submod
// case _ => ()
// buf.toSeq

// def emit(design: ElaboratedDesign): String =
// val sb = new StringBuilder
// sb.append(s"module ${design.name}:\n")
// design.ports.foreach(p => sb.append(s"  $p\n"))
// sb.append("\n")
// design.body.foreach(s => sb.append(s"  $s\n"))
// sb.toString

// def emitAll: String =
// elaborated.values.map(f => emit(Await.result(f, Duration.Inf))).mkString("\n")

// object Macros:
// def findEnclosingValName(using q: Quotes): Option[String] =
// import q.reflect.*
// val nodeTpe = TypeRepr.of[Node[?]]
// val moduleTpe = TypeRepr.of[Module]

// def isNodeVal(sym: Symbol): Boolean =
// if !sym.isTerm || sym.isPackageDef then false
// else
// sym.tree match
// case v: ValDef =>
// val tpe = v.tpt.tpe
// report.info(s"isNodeVal tpe=${tpe}")
// tpe != null && ((tpe <:< nodeTpe) || (tpe <:< moduleTpe))
// case _ => false

// def searchOwners(sym: Symbol, crossedSyntheticDef: Boolean): Option[String] =
// val flagStr = sym.flags.show
// val nodeMatch = isNodeVal(sym)
// report.info(s"findEnclosingValName visiting=${sym.fullName} flags=$flagStr nodeVal=$nodeMatch crossedSynthetic=$crossedSyntheticDef isTerm=${sym.isTerm} isPackageDef=${sym.isPackageDef}")
// if sym.isNoSymbol then
// report.info("findEnclosingValName reached no symbol")
// None
// else if sym.isValDef && nodeMatch && !sym.flags.is(Flags.Synthetic) && !sym.flags.is(Flags.Artifact) then
// if crossedSyntheticDef then
// report.info(s"findEnclosingValName dropping ${sym.name} due to synthetic boundary")
// None
// else
// report.info(s"findEnclosingValName found=${sym.name}")
// Some(sym.name)
// else
// val crossed = crossedSyntheticDef || (sym.isDefDef && (sym.flags.is(Flags.Synthetic) || sym.flags.is(Flags.Artifact)))
// searchOwners(sym.owner, crossed)
// searchOwners(Symbol.spliceOwner.owner, false)

// def ioImpl[T <: ValueType: Type](
// t: Expr[T],
// mod: Expr[Module]
// )(using q: Quotes): Expr[Node[T]] =
// import q.reflect.*
// val nameOpt = findEnclosingValName
// '{
// val tpe = $t
// val m = $mod
// val baseName = m.getBuilder.allocateName(${Expr(nameOpt)}, "io")
// val portDecls = emitPortDecl(baseName, tpe)
// portDecls.foreach(m.getBuilder.addPort)
// Node(tpe, NodeKind.IO, Some(baseName), None, baseName)
// }

// def wireImpl[T <: ValueType: Type](
// t: Expr[T],
// mod: Expr[Module]
// )(using q: Quotes): Expr[Node[T]] =
// import q.reflect.*
// val nameOpt = findEnclosingValName
// '{
// val tpe = $t
// val m = $mod
// val wireName = m.getBuilder.allocateName(${Expr(nameOpt)}, "wire")
// m.getBuilder.addBody(s"wire $wireName : ${formatType(tpe)}")
// Node(tpe, NodeKind.Wire, Some(wireName), None, wireName)
// }

// def regImpl[T <: ValueType: Type](
// t: Expr[T],
// mod: Expr[Module]
// )(using q: Quotes): Expr[Node[T]] =
// import q.reflect.*
// val nameOpt = findEnclosingValName
// '{
// val tpe = $t
// val m = $mod
// val regName = m.getBuilder.allocateName(${Expr(nameOpt)}, "reg")
// m.getBuilder.addBody(s"reg $regName : ${formatType(tpe)}, clock")
// Node(tpe, NodeKind.Reg, Some(regName), None, regName)
// }

// def litImpl[T <: ValueType: Type](
// t: Expr[T],
// payload: Expr[HostTypeOf[T]],
// mod: Expr[Module]
// )(using q: Quotes): Expr[Node[T]] =
// import q.reflect.*
// val nameOpt = findEnclosingValName
// '{
// val tpe = $t
// val value = $payload
// val m = $mod
// val litName = m.getBuilder.allocateName(${Expr(nameOpt)}, "lit")
// m.getBuilder.addBody(s"node $litName = ${formatLiteral(tpe, value)}")
// Node(tpe, NodeKind.Lit, Some(litName), Some(value), litName)
// }

// def moduleInstImpl[M <: Module: Type](
// gen: Expr[M],
// parent: Expr[Module]
// )(using q: Quotes): Expr[M] =
// import q.reflect.*
// val nameOpt = findEnclosingValName
// val key = buildModuleKey[M](gen)
// '{
// val submod = $gen
// val m = $parent
// submod.setElabKey(${Expr(key)})
// val instName = m.getBuilder.allocateName(${Expr(nameOpt)}, "inst")
// m.getBuilder.addBody(s"inst $instName of ${submod.moduleName}")
// setSubmodulePrefix(submod, instName)
// submod
// }

// private def buildModuleKey[M: Type](gen: Expr[M])(using Quotes): String =
// import quotes.reflect.*
// def extractArgs(term: Term): List[Term] =
// term match
// case Inlined(_, _, expr) => extractArgs(expr)
// case Apply(_, args) => args
// case TypeApply(fun, _) => extractArgs(fun)
// case _ => Nil

// def renderTerm(term: Term): String =
// term match
// case Literal(c) => c.value match
// case null => "null"
// case b: Boolean => b.toString
// case b: Byte => b.toString
// case s: Short => s.toString
// case i: Int => i.toString
// case l: Long => l.toString
// case f: Float => f.toString
// case d: Double => d.toString
// case s: String => s
// case other => other.toString
// case _ => term.show(using Printer.TreeShortCode)

// val tpeStr = Type.show[M]
// val args = extractArgs(gen.asTerm)
// val rendered = args.map(renderTerm)
// val body = s"$tpeStr|${rendered.mkString(",")}"
// val hash = MurmurHash3.stringHash(body)
// s"$tpeStr#$hash"

// def emitPortDecl(name: String, tpe: ValueType, prefix: String = ""): Seq[String] =
// val fullName = if prefix.isEmpty then name else s"$prefix.$name"
// tpe match
// case u: UInt =>
// val dirStr = if u.dir == Direction.In then "input" else "output"
// Seq(s"$dirStr $fullName : UInt<${u.w.value}>")
// case b: Bool =>
// val dirStr = if b.dir == Direction.In then "input" else "output"
// Seq(s"$dirStr $fullName : Bool")
// case bundle: Bundle =>
// val p = bundle.asInstanceOf[Product]
// (0 until p.productArity).flatMap { i =>
// val fieldName = p.productElementName(i)
// val fieldVal = p.productElement(i).asInstanceOf[ValueType]
// emitPortDecl(fieldName, fieldVal, fullName)
// }

// def setSubmodulePrefix(mod: Module, prefix: String): Unit =
// val clazz = mod.getClass
// for field <- clazz.getDeclaredFields do
// field.setAccessible(true)
// field.get(mod) match
// case node: Node[?] if node.kind == NodeKind.IO =>
// node.setRef(s"$prefix.${node.ref}")
// case _ => ()

// def formatType(tpe: ValueType): String = tpe match
// case u: UInt => s"UInt<${u.w.value}>"
// case b: Bool => "Bool"
// case bundle: Bundle =>
// val p = bundle.asInstanceOf[Product]
// val fields = (0 until p.productArity).map { i =>
// val fieldName = p.productElementName(i)
// val fieldVal = p.productElement(i).asInstanceOf[ValueType]
// val fieldDir = fieldVal match
// case u: UInt => if u.dir == Direction.In then "flip " else ""
// case b: Bool => if b.dir == Direction.In then "flip " else ""
// case _: Bundle => ""
// s"$fieldDir$fieldName : ${formatType(fieldVal)}"
// }
// s"{ ${fields.mkString(", ")} }"

// def formatLiteral(tpe: ValueType, value: Any): String = tpe match
// case u: UInt => s"UInt<${u.w.value}>($value)"
// case _: Bool => s"Bool($value)"
// case _ => value.toString

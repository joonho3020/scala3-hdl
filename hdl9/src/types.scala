package hdl9

import scala.deriving.*
import scala.compiletime.*
import scala.util.NotGiven
import scala.quoted.*
import scala.language.dynamics
import scala.NamedTuple.*

final class Reg[T](val t: T) extends Dynamic:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  transparent inline def selectDynamic(inline name: String): Reg[? <: ValueType] =
    ${ RegMacros.selectDynamicImpl[T]('t, 'name) }

  override def toString(): String =
    s"Reg(${t})"

object RegMacros:
  def selectDynamicImpl[T: Type](
    t: Expr[T], nameExpr: Expr[String]
  )(using Quotes): Expr[Reg[? <: ValueType]] =
    import quotes.reflect.*
    nameExpr.value match
      case Some(fieldName) =>
        val selected =
          try Select.unique(t.asTerm, fieldName)
          catch
            case _: Throwable =>
              report.errorAndAbort(s"${Type.show[T]} has no field '$fieldName'")

        val widenedFieldTpe = selected.tpe.widenTermRefByName
        if !(widenedFieldTpe <:< TypeRepr.of[ValueType]) then
          report.errorAndAbort(s"Field '$fieldName' type ${widenedFieldTpe.show} is not a subtype of ValueType")

        val selectedExpr = selected.asExpr
        widenedFieldTpe.asType match
          case '[ft] => '{ new Reg[ft & ValueType](${selectedExpr}.asInstanceOf[ft & ValueType]) }
      case None =>
        quotes.reflect.report.errorAndAbort("selectDynamic requires a literal field name")

// Derive host-side representation types for HDL ValueTypes, including structural bundles
type HostTypeOf[T] = HostOf[T & ValueType]#Out

trait HostOf[T <: ValueType]:
  type Out

object HostOf:
  given HostOf[UInt] with
    type Out = Int

  given HostOf[Bool] with
    type Out = Boolean

  given [A <: ValueType](using ha: HostOf[A]): HostOf[Vec[A]] with
    type Out = Seq[ha.Out]

  inline given [T <: Bundle]: HostOf[T] = ${ HostOfMacros.hostOfBundleImpl[T] }

object HostOfMacros:
  def hostOfBundleImpl[T: Type](using Quotes): Expr[HostOf[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    if !(tpe <:< TypeRepr.of[Bundle]) then
      report.errorAndAbort(s"HostTypeOf can only be derived for Bundle (got: ${tpe.show})")

    // Collect bundle fields: supports both classes with vals and structural Refinements
    def classFields(sym: Symbol, tref: TypeRepr): List[(String, TypeRepr)] =
      val syms = sym.fieldMembers ++ sym.caseFields ++ sym.methodMembers
      syms
        .distinctBy(_.name)
        .flatMap { s =>
          val mt = tref.memberType(s)
          val ft = mt match
            case mt: MethodType => mt.resType
            case pt: PolyType   => pt.resType
            case other          => other
          if ft <:< TypeRepr.of[ValueType] then Some((s.name, ft)) else None
        }

    def refinementFields(tr: TypeRepr): List[(String, TypeRepr)] = tr match
      case Refinement(parent, name, info) =>
        val base = refinementFields(parent)
        val infoTpe = info match
          case mt: MethodType => mt.resType
          case pt: PolyType   => pt.resType
          case other          => other
        if infoTpe <:< TypeRepr.of[ValueType] then base :+ (name -> infoTpe) else base
      case _ => Nil

    val fields: List[(String, TypeRepr)] =
      if tpe.typeSymbol == Symbol.noSymbol then refinementFields(tpe)
      else classFields(tpe.typeSymbol, tpe)

    // Build labelled named-tuple type: ("a" ->> HostTypeOf[A]) *: ... *: EmptyTuple
    def labelledCons(label: String, valueT: TypeRepr, tailT: TypeRepr): TypeRepr =
      val lTpe = ConstantType(StringConstant(label))
      (lTpe.asType, valueT.asType, tailT.asType) match
        case ('[l], '[v], '[t]) =>
          TypeRepr.of[(l ->> v) *: t]

    val outTupleTpe: TypeRepr =
      fields.foldRight(TypeRepr.of[EmptyTuple]) { case ((label, fT), tail) =>
        val hostFieldT = fT.asType match
          case '[ft] => TypeRepr.of[HostTypeOf[ft & ValueType]]
        labelledCons(label, hostFieldT, tail)
      }

    outTupleTpe.asType match
      case '[o] => '{ new HostOf[T] { type Out = o } }

final class Lit[T](private val payload: HostTypeOf[T]) extends Dynamic:
  transparent inline def selectDynamic(inline name: String): Lit[? <: ValueType] =
    ${ LitMacros.selectDynamicImpl[T]('payload, 'name) }

  transparent inline def get: HostTypeOf[T] = payload

object Lit:
  inline def apply[T <: ValueType](inline v: HostTypeOf[T]): Lit[T] =
    new Lit[T](v)

object LitVecOps:
  extension [A <: ValueType](lv: Lit[Vec[A]])
    def apply(index: Int): Lit[A] =
      val seq = lv.get.asInstanceOf[Seq[HostTypeOf[A]]]
      new Lit[A](seq(index))

    def apply(start: Int, end: Int): Lit[Vec[A]] =
      val seq = lv.get.asInstanceOf[Seq[HostTypeOf[A]]]
      new Lit[Vec[A]](seq.slice(start, end + 1).asInstanceOf[HostTypeOf[Vec[A]]])

object LitMacros:
  def selectDynamicImpl[T: Type](
    payload: Expr[HostTypeOf[T]], nameExpr: Expr[String]
  )(using Quotes): Expr[Lit[? <: ValueType]] =
    import quotes.reflect.*
    nameExpr.value match
      case Some(fieldName) =>
        val tpe = TypeRepr.of[T]
        if !(tpe <:< TypeRepr.of[ValueType]) then
          report.errorAndAbort(s"Lit can only select from ValueType (got: ${tpe.show})")

        // For bundle-like types, ensure the field exists and is a ValueType
        if tpe <:< TypeRepr.of[Bundle] then
          // Determine ordered list of fields and their types for T
          def classFields(sym: Symbol, tref: TypeRepr): List[(String, TypeRepr)] =
            val syms = sym.fieldMembers ++ sym.caseFields ++ sym.methodMembers
            // Deduplicate by name keeping first occurrence, filter value-like members
            syms
              .distinctBy(_.name)
              .flatMap { s =>
                val mt = tref.memberType(s)
                val ft = mt match
                  case mt: MethodType => mt.resType
                  case pt: PolyType => pt.resType
                  case other => other
                if ft <:< TypeRepr.of[ValueType] then Some((s.name, ft)) else None
              }

          def refinementFields(tr: TypeRepr): List[(String, TypeRepr)] = tr match
            case Refinement(parent, name, info) =>
              val base = refinementFields(parent)
              // Only keep ValueType-like fields
              val infoTpe = info match
                case mt: MethodType => mt.resType
                case pt: PolyType => pt.resType
                case other => other
              if infoTpe <:< TypeRepr.of[ValueType] then base :+ (name -> infoTpe) else base
            case _ => Nil

          val fields: List[(String, TypeRepr)] =
            if tpe.typeSymbol == Symbol.noSymbol then refinementFields(tpe)
            else classFields(tpe.typeSymbol, tpe)

          val idx = fields.indexWhere(_._1 == fieldName)
          if idx < 0 then
            report.errorAndAbort(s"${tpe.show} has no field '$fieldName'")

          val fieldTpe = fields(idx)._2
          if !(fieldTpe <:< TypeRepr.of[ValueType]) then
            report.errorAndAbort(s"Field '$fieldName' type ${fieldTpe.show} is not a subtype of ValueType")

          val selectedHost =
            '{
              val p = $payload.asInstanceOf[Product]
              p.productElement(${Expr(idx)}).asInstanceOf[Any]
            }.asTerm

          fieldTpe.asType match
            case '[ft] => '{ new Lit[ft & ValueType](${selectedHost.asExpr}.asInstanceOf[HostTypeOf[ft & ValueType]]) }
        else
          report.errorAndAbort(s"Field selection only supported for Lit[T] where T <: Bundle (got: ${tpe.show})")
      case None =>
        report.errorAndAbort("selectDynamic requires a literal field name")

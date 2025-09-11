package hdl9

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*
import scala.language.dynamics

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

type HostTypeOf[T] = T match
  case UInt    => Int
  case Bool    => Boolean
  case Vec[t]  => Seq[HostTypeOf[t & ValueType]]
  case _       => NamedTuple.Map[NamedTuple.From[T], [X] =>> HostTypeOf[X & ValueType]]

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
      val seq = lv.get
      new Lit[A](seq(index))

    def apply(start: Int, end: Int): Lit[Vec[A]] =
      val seq = lv.get
      new Lit[Vec[A]](seq.slice(start, end + 1))

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

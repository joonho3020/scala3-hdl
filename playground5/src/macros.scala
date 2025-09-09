package playground5

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.quoted.*
import scala.language.dynamics


final class Wrapper[T](val t: T) extends Dynamic:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Wrapper[X & ValueType]]

  transparent inline def selectDynamic(inline name: String): Wrapper[? <: ValueType] =
    ${ WrapperMacros.selectDynamicImpl[T]('t, 'name) }

  override def toString(): String =
    s"Wrapper(${t})"

object WrapperMacros:
  def selectDynamicImpl[T: Type](t: Expr[T], nameExpr: Expr[String])(using Quotes): Expr[Wrapper[? <: ValueType]] =
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
          case '[ft] => '{ new Wrapper[ft & ValueType](${selectedExpr}.asInstanceOf[ft & ValueType]) }
      case None =>
        quotes.reflect.report.errorAndAbort("selectDynamic requires a literal field name")

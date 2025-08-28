package hdl

import scala.compiletime.{erasedValue, constValue, summonInline}
import scala.deriving.*
import scala.compiletime.ops.int.*
import scala.annotation.targetName
import scala.compiletime.erasedValue
import scala.quoted.*

trait Bundle

object Bundle:
  inline def wire[A <: Bundle]: A = ${ BundleMacros.wireImpl[A] }

  inline def lit[A <: Bundle](inline values: Tuple): A =
    ${ BundleMacros.litImpl[A]('values) }

  inline def dpi[A <: Bundle](inline prefix: String): A =
    ${ BundleMacros.dpiImpl[A]('prefix) }

private object BundleMacros:
  def wireImpl[A <: Bundle: Type](using Quotes): Expr[A] =
    import quotes.reflect.*
    val tpe = TypeRepr.of[A]

    val classSymbol = tpe.classSymbol.getOrElse(
      report.errorAndAbort(s"Cannot find class symbol for ${tpe.show}")
    )
    val fields = classSymbol.declaredFields.filter(_.isValDef)
    val fieldDefs = fields.map { field =>
      val fieldName = field.name
      val fieldType = tpe.memberType(field)
      val wireExpr = createWireForType(fieldType)
      ValDef(field, Some(wireExpr.asTerm))
    }
    val parents = List(TypeTree.of[A])
    val body = fieldDefs
    val classDef = ClassDef.copy(Symbol.spliceOwner, "AnonBundle", parents, body)
    val newExpr = New(TypeIdent(classDef.symbol)).select(classDef.symbol.primaryConstructor).appliedToNone
    newExpr.asExprOf[A]

  private def createWireForType(tpe: TypeRepr)(using Quotes): Expr[Any] =
    import quotes.reflect.*

    // Check if the type is a Sig type
    if tpe <:< TypeRepr.of[Sig[?]] then
      // For Sig types, create a Wire instance
      '{ Wire() }
    else if tpe <:< TypeRepr.of[Bundle] then
      // For Bundle types, recursively call Bundle.wire
      '{ Bundle.wire }
    else
      // For other types, try to create a default value
      report.errorAndAbort(s"Cannot create wire for type ${tpe.show}")

  def litImpl[A <: Bundle: Type](values: Expr[Tuple])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.errorAndAbort("Bundle.lit macro not yet implemented (scaffold)")

  def dpiImpl[A <: Bundle: Type](prefix: Expr[String])(using Quotes): Expr[A] =
    import quotes.reflect.*
    report.errorAndAbort("Bundle.dpi macro not yet implemented (scaffold)")

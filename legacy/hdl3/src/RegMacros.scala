package hdl3

import scala.quoted.*

object RegMacros:
  def selectFieldFromReg[V <: ValueType : Type](reg: Expr[Reg[V]], nameExpr: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*

    val nameOpt = nameExpr.value
    val vTpe = TypeRepr.of[V]
    if !(vTpe <:< TypeRepr.of[Bundle]) then
      report.errorAndAbort(s"Field selection only supported for Reg[T] where T <: Bundle (got: ${vTpe.show})")
    val sym = vTpe.typeSymbol
    if !sym.flags.is(Flags.Case) then
      report.errorAndAbort(s"Reg view only supports case class bundles (got: ${sym.fullName})")

    val fields = sym.caseFields
    nameOpt match
      case Some(nm) =>
        fields.find(_.name == nm) match
          case Some(fsym) =>
            val ftpe0 = vTpe.memberType(fsym) match
              case mt: MethodType => mt.resType
              case other => other
            if !(ftpe0 <:< TypeRepr.of[ValueType]) then
              report.errorAndAbort(s"Field '$nm' of ${sym.fullName} is not a ValueType: ${ftpe0.show}")
            ftpe0.asType match
              case '[ft] =>
                val valueSel = Select.unique(reg.asTerm, "value")
                val fieldSel = Select.unique(valueSel, nm)
                '{ new Reg[ft & ValueType](${ fieldSel.asExprOf[ft & ValueType] }) }
          case None =>
            report.errorAndAbort(s"No field '$nm' in ${sym.fullName}")
      case None =>
        report.errorAndAbort("Field name must be a known literal")


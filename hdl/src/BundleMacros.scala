package hdl

import scala.quoted.*

object BundleMacros:
  def bundleLitImpl[B: Type](elemsExpr: Expr[Seq[(String, Any)]])(using Quotes): Expr[Bundle] =
    import quotes.reflect.*

    val bundleTpe = TypeRepr.of[B]
    val bundleSym = bundleTpe.typeSymbol

    val declaredVals: Map[String, TypeRepr] =
      bundleSym.declaredFields
        .map { sym =>
          val name = sym.name
          val tpe = bundleTpe.memberType(sym)
          name -> tpe
        }
        .filter { case (_, tpe) => tpe <:< TypeRepr.of[Signal] || tpe <:< TypeRepr.of[Bundle] }
        .toMap

    def extractLiteralString(expr: Expr[String]): Option[String] = expr match
      case Expr(value) => Some(value)
      case _           => None

    elemsExpr match
      case Varargs(args) =>
        val validatedPairs: List[Expr[(String, Any)]] = args.map { pairExpr =>
          def handle(k: Expr[String], v: Expr[Any]): Expr[(String, Any)] =
            val vTerm = v.asTerm
            extractLiteralString(k) match
              case None =>
                report.errorAndAbort("Bundle.lit requires string literal field names")
              case Some(fieldName) =>
                declaredVals.get(fieldName) match
                  case None =>
                    val known = declaredVals.keys.toList.sorted.mkString(", ")
                    report.errorAndAbort(s"Field '$fieldName' is not a member of ${bundleSym.name}. Known fields: [$known]")
                  case Some(expectedTpe) =>
                    val providedTpe = vTerm.tpe
                    val ok =
                      if expectedTpe <:< TypeRepr.of[Bundle] then providedTpe <:< TypeRepr.of[Bundle]
                      else providedTpe <:< expectedTpe
                    if !ok then
                      report.errorAndAbort(s"Field '$fieldName' expects value of type ${expectedTpe.show}, but got ${providedTpe.show}")
                    '{ Tuple2($k, $v.asInstanceOf[Any]) }

          pairExpr match
            // Tuple literal
            case '{ ($k: String, $v) } => handle(k, v)
            // ArrowAssoc '->' syntax
            case '{ ($k: String) -> ($v) } => handle(k, v)
            case _ =>
              report.errorAndAbort("Invalid argument to Bundle.lit; expected (String, Signal) pairs")
        }.toList

        '{ new Bundle(${ Varargs(validatedPairs) }: _*) }
      case _ =>
        report.errorAndAbort("Invalid varargs for Bundle.lit")

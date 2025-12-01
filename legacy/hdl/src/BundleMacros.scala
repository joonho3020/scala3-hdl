package hdl

import scala.quoted.*

object BundleMacros:
  def bundleLitImpl[B: Type](
    elemsExpr: Expr[Seq[(String, Signal)]]
  )(using Quotes): Expr[Any] =
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

    println(s"${bundleTpe.show}: ${bundleSym}, ${bundleSym.declaredFields}")
    bundleSym.declaredFields.foreach(sym => {
      println(s"${sym.name} ${bundleTpe.memberType(sym)}")
    })

    val resultExpr: Expr[Any] = elemsExpr match
      case Varargs(args) =>
        // Build validated pairs and collect field type info for structural refinement
        val validatedAndInfos: List[(Expr[(String, Signal)], Option[(String, TypeRepr)])] = args.map { pairExpr =>
          def handle(k: Expr[String], v: Expr[Signal]): (Expr[(String, Signal)], Option[(String, TypeRepr)]) =
            val vTerm = v.asTerm

            k.value match
              case Some(fieldName) =>
                declaredVals.get(fieldName) match
                  case None =>
                    val known = declaredVals.keys.toList.sorted.mkString(", ")
                    report.errorAndAbort(s"Field '$fieldName' is not a member of ${bundleSym.name}. Known fields: [$known]")
                  case Some(expectedTpe) =>
                    val providedTpe = vTerm.tpe
                    println(s"providedTpe ${providedTpe} ${providedTpe.show}")
                    val ok =
                      if expectedTpe <:< TypeRepr.of[UInt] then providedTpe <:< TypeRepr.of[UIntLit]
                      else if expectedTpe <:< TypeRepr.of[Bundle] then providedTpe <:< TypeRepr.of[Bundle]
                      else providedTpe <:< expectedTpe
                    if !ok then
                      report.errorAndAbort(s"Field '$fieldName' expects value of type ${expectedTpe.show}, but got ${providedTpe.show}")
                    val pairExpr: Expr[(String, Signal)] = '{ Tuple2($k, $v.asInstanceOf[Signal]) }
                    // For refinement, keep the provided type (more precise), but erase to a non-singleton
                    val refineTpe = providedTpe.widen
                    (pairExpr, Some(fieldName -> refineTpe))
              case None =>
                report.errorAndAbort("Bundle.lit requires string literal field names")

          pairExpr match
            // ArrowAssoc '->' syntax
            case '{ ($k: String) -> ($v: Signal) } => handle(k, v)
            case _ =>
              report.errorAndAbort("Invalid argument to Bundle.lit; expected (String, Signal) pairs")
        }.toList

        val validatedPairs: List[Expr[(String, Signal)]] = validatedAndInfos.map(_._1)
        val fieldInfos: List[(String, TypeRepr)] = validatedAndInfos.flatMap(_._2)

        // Construct structural refinement type: Bundle { val f1: T1; val f2: T2; ... }
        val refinedTpe: TypeRepr = fieldInfos.foldLeft(TypeRepr.of[Bundle]) { case (acc, (name, tpe)) =>
          Refinement(acc, name, tpe)
        }

        val baseExpr: Expr[Bundle] = '{ new Bundle(${ Varargs(validatedPairs) }*) }

        refinedTpe.asType match
          case '[t] => '{ $baseExpr.asInstanceOf[t] }
      case _ =>
        report.errorAndAbort("Invalid varargs for Bundle.lit")

    resultExpr

  def bundleDynamicUnknownMethod(methodExpr: Expr[String])(using Quotes): Expr[Any] =
    import quotes.reflect.*
    methodExpr.value match
      case Some(name) => report.errorAndAbort(s"Unknown method '$name' on Bundle companion. Only 'lit' is supported.")
      case None       => report.errorAndAbort("Unknown method on Bundle companion.")

  def deriveBundleCompanionConversion[M: Type](using Quotes): Expr[Any] =
    import quotes.reflect.*
    val mTpe = TypeRepr.of[M]
    val mSym = mTpe.typeSymbol
    if !mSym.is(Module) then
      report.errorAndAbort(s"Expected a companion module type, got: ${mTpe.show}")
    val compClass = mSym.companionClass
    if !compClass.exists then
      report.errorAndAbort(s"No companion class for module: ${mSym.name}")
    val clsTpe = compClass.typeRef
    if !(clsTpe <:< TypeRepr.of[Bundle]) then
      report.errorAndAbort(s"Companion class ${compClass.name} is not a subclass of hdl.Bundle")

    clsTpe.asType match
      case '[b <: Bundle] =>
        '{
          new scala.Conversion[M, hdl.BundleCompanionDynamic[b]] {
            def apply(m: M): hdl.BundleCompanionDynamic[b] = new hdl.BundleCompanionDynamic[b]
          }
        }
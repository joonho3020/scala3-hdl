package hdl6


import scala.quoted.*
import scala.compiletime.*

object TupleSchema:
  type Of[T] = Schema[T]#Out

  trait Schema[T]:
    type Out
    def value(t: T): Out

  transparent inline def derived[T]: Schema[T] = ${ schemaImpl[T] }
  inline def valueOf[T](t: T): Of[T]          = ${ valueImpl('t) }
  inline def coerce[T](v: Any): Of[T]         = ${ coerceImpl[T]('v) }

  // ===== impl =====
  private def schemaImpl[T: Type](using q: Quotes): Expr[Schema[T]] =
    import q.reflect.*
    val (outT, mkValue) = build[T](TypeRepr.of[T].dealias.simplified)
    outT.asType match
      case '[o] =>
        '{
          new Schema[T]:
            type Out = o
            def value(t: T): o = ${ '{ ${ mkValue('t).asExpr } .asInstanceOf[o] } }
        }

  private def valueImpl[T: Type](t: Expr[T])(using q: Quotes): Expr[Of[T]] =
    import q.reflect.*
    val (outT, mkValue) = build[T](TypeRepr.of[T].dealias.simplified)
    outT.asType match
      case '[o] => '{ ${ mkValue(t).asExpr } .asInstanceOf[o] }.asExprOf[TupleSchema.Of[T]]

  private def coerceImpl[T: Type](v: Expr[Any])(using q: Quotes): Expr[Of[T]] =
    import q.reflect.*
    val (outT, _) = build[T](TypeRepr.of[T].dealias.simplified)
    outT.asType match
      case '[o] => '{ $v.asInstanceOf[o] }.asExprOf[TupleSchema.Of[T]]

  // ---- reflection core ----
  private def build[T: Type](using q: Quotes)(
    tr: q.reflect.TypeRepr
  ): (q.reflect.TypeRepr, Expr[T] => q.reflect.Term) =
    import q.reflect.*
    val signalT     = TypeRepr.of[Signal]
    val bundleLikeT = TypeRepr.of[BundleLike]

    def tupleType(elems: List[TypeRepr]): TypeRepr =
      elems.foldRight(TypeRepr.of[EmptyTuple])((h, acc) => AppliedType(TypeRepr.of[*:], List(h, acc)))

    def tupleValueTyped(items: List[(TypeRepr, Term)]): Term =
      items.foldRight('{ EmptyTuple: Tuple }.asTerm) { case ((hT, hTerm), acc) =>
        hT.asType match
          case '[h] =>
            val hExpr = hTerm.asExprOf[h]
            val accExpr = acc.asExprOf[Tuple]
            '{ $hExpr *: $accExpr }.asTerm
      }

    def methodResult(t: TypeRepr): TypeRepr = t match
      case MethodType(_, _, res) => res
      case other                 => other

    def gettersInSrcOrder(cls: Symbol, ownerT: TypeRepr): List[Symbol] =
      val direct0Arg =
        cls.memberMethods.filter { m =>
          !m.flags.is(Flags.Private | Flags.Protected | Flags.Synthetic) &&
          !m.isType && m.paramSymss.flatten.isEmpty && m.owner == cls
        }
      // stable order by source position if available; else keep as-is
      def posStart(s: Symbol): Int =
        s.tree match
          case d: DefDef => d.pos.start
          case _         => Int.MaxValue
      direct0Arg.sortBy(posStart)

    def go(t0: TypeRepr): (TypeRepr, Expr[Any] => Term) =
      t0.dealias.simplified match
        case r: Refinement =>
          val fields = collectRefinements(r).filter { case (_, ft) => ft <:< signalT || ft <:< bundleLikeT }
          val elems  = fields.map { case (n, ft) =>
            if ft <:< bundleLikeT then
              val (subT, subGet) = go(ft)
              (subT, (x: Expr[Any]) => subGet(Select.unique(x.asTerm, n).asExpr))
            else
              (ft,  (x: Expr[Any]) => Select.unique(x.asTerm, n))
          }
          (TypeRepr.of[Tuple], (x: Expr[Any]) => tupleValueTyped(elems.map { case (tpe, f) => (tpe, f(x)) }))

        case AppliedType(base, _) =>
          go(base)

        case base =>
          base.classSymbol match
            case None => (base, (_: Expr[Any]) => '{ EmptyTuple }.asTerm)
            case Some(cls) =>
              def fieldPosStart(s: Symbol): Int =
                s.tree match
                  case v: ValDef => v.pos.start
                  case _         => Int.MaxValue
              val fields = cls.declaredFields
                .filter { f =>
                  !f.flags.is(Flags.Private | Flags.Protected | Flags.Synthetic) && f.owner == cls
                }
                .sortBy(fieldPosStart)
              val sigs = fields.flatMap { f =>
                val rt = base.memberType(f)
                if rt <:< signalT || rt <:< bundleLikeT then Some((f.name, rt)) else None
              }
              val elems = sigs.map { case (n, rt) =>
                if rt <:< bundleLikeT then
                  val (subT, subGet) = go(rt)
                  (subT, (x: Expr[Any]) => subGet(Select.unique(x.asTerm, n).asExpr))
                else
                  (rt,  (x: Expr[Any]) => Select.unique(x.asTerm, n))
              }
              (TypeRepr.of[Tuple], (x: Expr[Any]) => tupleValueTyped(elems.map { case (tpe, f) => (tpe, f(x)) }))

    def collectRefinements(r: Refinement): List[(String, TypeRepr)] =
      def loop(t: TypeRepr, acc: List[(String, TypeRepr)]): List[(String, TypeRepr)] =
        t match
          case Refinement(parent, name, info) =>
            val ft = info match
              case TypeBounds(_, hi) => hi
              case ByNameType(t1)    => t1
              case t1                => t1
            loop(parent, (name, ft) :: acc)
          case _ => acc.reverse
      loop(r, Nil)

    go(tr)

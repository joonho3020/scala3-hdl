package hdl6


import scala.quoted.*
import scala.compiletime.*

type :-> [K <: String, V] = (K, V)

trait BundleSchema[T]:
  type Out
  def labels: Tuple

object BundleSchema:
  type Of[T] = BundleSchema[T]#Out

  transparent inline def derived[T]: BundleSchema[T] = ${ schemaImpl[T] }

  inline def labelsOf[T]: Tuple = ${ labelsImpl[T] }

  // -------------------- Macro impl --------------------
  private def schemaImpl[T: Type](using q: Quotes): Expr[BundleSchema[T]] =
    import q.reflect.*
    val tpe = TypeRepr.of[T].dealias.simplified

    // Compute (type Out, term labels) together so we only reflect once.
    val (outTpe, labelsExpr) = buildSchemaFor(tpe)

    outTpe.asType match
      case '[o] =>
        '{
          new BundleSchema[T]:
            type Out = o
            val labels: Tuple = $labelsExpr
        }

  private def labelsImpl[T: Type](using q: Quotes): Expr[Tuple] =
    import q.reflect.*
    val (_, labels) = buildSchemaFor(TypeRepr.of[T].dealias.simplified)
    labels

  // ---- core reflection ----
  private def buildSchemaFor(using q: Quotes)(t: q.reflect.TypeRepr): (q.reflect.TypeRepr, Expr[Tuple]) =
    import q.reflect.*

    // Named pair type: "K" :-> V  ==>  (:->)["K", V]
    def pairType(label: String, v: TypeRepr): TypeRepr =
      AppliedType(TypeRepr.of[:->], List(ConstantType(StringConstant(label)), v))

    // Tuple cons type from a list of TypeRepr
    def mkTupleType(elems: List[TypeRepr]): TypeRepr =
      elems.foldRight(TypeRepr.of[EmptyTuple]) { (h, acc) => AppliedType(TypeRepr.of[*:], List(h, acc)) }

    // Tuple literal Expr[Tuple] from a list of Expr[Any] (labels are Strings)
    def mkTupleExpr(ss: List[Expr[Any]]): Expr[Tuple] =
      ss.foldRight('{ EmptyTuple: Tuple }) { (h, acc) => '{ $h *: $acc } }

    def isGoodField(sym: Symbol): Boolean =
      // public, non-synthetic, non-private, 0-arg getter (for class) OR a val in refinement
      !sym.flags.is(Flags.Private | Flags.Protected | Flags.Synthetic)
        && !sym.isType
        && sym.paramSymss.flatten.isEmpty

    // Recursively compute Out type + labels for an arbitrary type
    def go(tr: TypeRepr): (TypeRepr, Expr[Tuple]) =
      println(s"tr.dealias.simplified ${tr.dealias.simplified}")
      tr.dealias.simplified match
        // Structural type: Refinement chain Selectable { val a: A; val b: B; ... }
        case r: Refinement =>
          val fields = collectRefinements(r) // (name, type)
          if fields.isEmpty then (tr, '{ EmptyTuple }) // not actually a record
          else
            val outs = fields.map { case (n, ft) =>
              val (ot, _) = go(ft)       // recurse type-level
              pairType(n, ot)
            }
            val labels = mkTupleExpr(fields.map { case (n, _) => Expr(n) })
            (mkTupleType(outs), labels)

        // Class / case class / object type
        case at @ AppliedType(tycon, _) =>
          go(tycon) // strip type params; schema depends on value members, not type args

        case tr0 =>
          val cls = tr0.classSymbol
          println(s"tr0.classSymbol ${tr0.classSymbol}")
          if cls.isEmpty then (tr0, '{ EmptyTuple })
          else
            // Prefer case fields if available; else, public 0-arg getters defined on the class
            val caseFields = cls.get.caseFields

            println(s"caseFields ${caseFields}")
            cls.get.memberFields.foreach(x => println(s"memberFields ${x}")) 
            cls.get.declaredFields.foreach(x => println(s"declaredFields ${x}")) 

            cls.get.declaredFields.foreach(x => println(s"declaredFields ${x.typeMembers}")) 
// val declaredSignals: List[Symbol] = cls.get.declaredFields.filter()

            val members: List[Symbol] =
              if caseFields.nonEmpty then caseFields
              else cls.get.memberMethods.filter(isGoodField)
                   .filter(m => m.owner == cls.get) // only directly-declared fields

            println(s"members ${members}")
            println(s" ${cls.get.memberMethods.filter(isGoodField)}")

            val pairs =
              members.flatMap { m =>
                val n  = m.name
                // discard compiler-artifact names (e.g., "copy", "productArity", etc.)
                if n == "copy" || n.startsWith("product") || n.startsWith("canEqual") then Nil
                else
                  val mt = tr0.memberType(m) match
                    case MethodType(_, _, res) => res
                    case t                     => t
                  val (ot, _) = go(mt)
                  pairType(n, ot) :: Nil
              }

            if pairs.isEmpty then (tr0, '{ EmptyTuple })
            else (mkTupleType(pairs), mkTupleExpr(members.map(m => Expr(m.name))))

    // Collect (name, type) from a Refinement chain
    def collectRefinements(r: Refinement): List[(String, TypeRepr)] =
      def loop(t: TypeRepr, acc: List[(String, TypeRepr)]): List[(String, TypeRepr)] =
        t match
          case Refinement(parent, name, info) =>
            val fieldT = info match
              case TypeBounds(lo, hi) => hi
              case ByNameType(t1)     => t1
              case t1                 => t1
            loop(parent, (name, fieldT) :: acc)
          case _ => acc.reverse
      loop(r, Nil)


    println(s"MACRO ${t.show}")
    go(t)

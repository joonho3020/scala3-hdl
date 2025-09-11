package playground7


import scala.quoted.*

inline def structuralToNamedTuple[A](value: A): Any = ${
  structuralToNamedTupleImpl('value)
}

private def structuralToNamedTupleImpl[A: Type](value: Expr[A])(using Quotes): Expr[Tuple] =
  import quotes.reflect.*

  val tpe = TypeRepr.of[A]
  val members = tpe.dealias.classSymbol.get.declarations.filter { s =>
      println(s"s ${s} ${s.privateWithin} ${s.protectedWithin} ${s.flags.is(Flags.Private)}")
     !(s.flags.is(Flags.Private) || s.flags.is(Flags.Protected)) && (s.isValDef || (s.isDefDef && !s.isClassConstructor && s.paramSymss.isEmpty))
  }

  println(s"members ${members}")

  val kvs: List[Expr[(String, Any)]] = members.map { m =>
    val name = Expr(m.name)
    val rhs  = Select(value.asTerm, m).asExprOf[Any]
    '{ ($name, $rhs) }
  }

  // Pattern 2: stage the *call* to another macro (makeTuple)
  // Varargs(...) builds an Expr[Seq[(String, Any)]]; `*` spreads it in the generated code.
  '{ makeTuple(${Varargs(kvs)}*) }

inline def makeTuple(inline pairs: (String, Any)*): Tuple =
  ${ makeTupleImpl('pairs) }

private def makeTupleImpl(pairsExpr: Expr[Seq[(String, Any)]])(using Quotes): Expr[Tuple] =
  pairsExpr match
    // Exact arity known at compile time → build a heterogeneous Tuple
    case Varargs(elems) =>
      def loop(es: List[Expr[(String, Any)]]): Expr[Tuple] = es match
        case Nil      => '{ EmptyTuple }
        case h :: tail => '{ $h *: ${ loop(tail) } }
      loop(elems.toList)

    // Fallback when it isn't Varargs (e.g., a runtime Seq)
    case _ =>
      '{ scala.runtime.Tuples.fromArray(${pairsExpr}.toArray[Object]) }

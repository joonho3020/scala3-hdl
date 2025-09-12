package playground7

import scala.quoted.*
import scala.NamedTuple.*

import scala.quoted.*

inline def structuralToTuple[A <: AnyRef](value: A): Any = ${
  structuralToTupleImpl('value)
}

private def structuralToTupleImpl[A <: AnyRef : Type](value: Expr[A])(using Quotes): Expr[Any] = {
  import quotes.reflect.*

  val members = TypeRepr.of[A].dealias.classSymbol.get.declarations.filter { s =>
    !(s.flags.is(Flags.Private) || s.flags.is(Flags.Protected)) && (s.isValDef || (s.isDefDef && !s.isClassConstructor && s.paramSymss.isEmpty))
  }

  def buildTupleExpr(remainingMembers: List[Symbol]): Expr[Tuple] = {
    remainingMembers match {
      case Nil => '{ EmptyTuple }
      case head :: tail =>
        val memberNameExpr = Expr(head.name)
        val restOfTupleExpr = buildTupleExpr(tail)
        head.tree.asInstanceOf[ValOrDefDef].tpt.tpe.asType match {
          case '[t] =>
            val memberValueExpr = Select(value.asTerm, head).asExprOf[t]
            val currentTupleElement = '{ ($memberNameExpr, $memberValueExpr) }
            '{ $currentTupleElement *: $restOfTupleExpr }
        }
    }
  }

  val finalTupleExpr = buildTupleExpr(members)

  finalTupleExpr.asTerm.tpe.asType match {
    case '[t] => finalTupleExpr.asExprOf[t]
  }
}

// A more robust version of `typeOf`
inline def typeOf(inline expr: Any): String = ${ typeOfImpl('expr) }

private def typeOfImpl(expr: Expr[Any])(using Quotes): Expr[String] = {
  import quotes.reflect.*
  // Inspect the type of the expression tree itself
  val typeString = expr.asTerm.tpe.widen.dealias.show
  Expr(typeString)
}

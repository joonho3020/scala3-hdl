package playground

import scala.quoted.*

def inspectCode(x: Expr[Any])(using Quotes): Expr[Any] =
  println(x.show)
  x

def pow(x: Double, n: Int): Double =
  if n == 0 then 1 else x * pow(x, n - 1)

def powerCode(
  x: Expr[Double],
  n: Expr[Int]
)(using Quotes): Expr[Double] =
  import quotes.reflect.report
  (x, n) match
    case (Expr(base), Expr(exponent)) =>
      val value: Double = pow(base, exponent)
      // NOTE: Creating expressions from values works for all primitive types,
      // tuples of any arity, Class, Array, Seq, Set, List, Map, Option,
      // Either, BigInt, BigDecimal, StringContext.
      // Other types can also work if a ToExpr is implemented for it
      Expr(value)
    case (Expr(_), _) =>
      // NOTE: The operations value, valueOrAbort, and Expr.unapply will work
      // for all primitive types,
      // tuples of any arity, Option, Seq, Set, Map, Either and StringContext.
      // Other types can also work if an FromExpr is implemented for it
      report.errorAndAbort("Expected a known value for the exponent, but was " + n.show, n)
    case _ =>
      report.errorAndAbort("Expected a known value for the base, but was " + x.show, x)

// Working with collections
def sumCode(nums: Expr[Seq[Int]])(using Quotes): Expr[Int] =
  import quotes.reflect.report
  nums match
    case Varargs(numberExprs) => // numberExprs: Seq[Expr[Int]]
      val numbers: Seq[Int] = numberExprs.map(_.valueOrAbort)
      Expr(numbers.sum)
    case _ =>
      report.errorAndAbort("Expected explicit varargs sequence, Notation `args*` is not supported", nums)


// NOTE: We have seen how to convert a List[Int] into an Expr[List[Int]] using Expr.apply.
// How about converting a List[Expr[Int]] into an Expr[List[Int]]?
// We mentioned that Varargs.apply can do this for sequences;
// likewise, for other collection types, corresponding methods are available:
// - Expr.ofList: Transform a List[Expr[T]] into Expr[List[T]]
// - Expr.ofSeq: Transform a Seq[Expr[T]] into Expr[Seq[T]] (just like Varargs)
// - Expr.ofTupleFromSeq: Transform a Seq[Expr[T]] into Expr[Tuple]
// - Expr.ofTuple: Transform a (Expr[T1], ..., Expr[Tn]) into Expr[(T1, ..., Tn)]


// generating blocks of code
def testCode(ignore: Expr[Boolean], computation: Expr[Unit])(using Quotes) =
  if ignore.valueOrAbort then Expr(false)
  else
    val x = List(computation)
    val ret = Expr.block(x, Expr(true))
    println(s"computation.show: ${computation.show}")
    println(s"ret.show: ${ret.show}")
    ret

trait FieldName:
  def uppercase: FieldName
  def lowercase: FieldName

def collectUsedMethods(func: Expr[FieldName => FieldName])(using Quotes): Expr[List[String]] =
  import quotes.reflect.*

  def collectCalls(term: Term, paramSym: Symbol, acc: List[String]): List[String] = term match
    case Inlined(_, _, t) => collectCalls(t, paramSym, acc)
    case Typed(t, _) => collectCalls(t, paramSym, acc)
    case Block(_, t) => collectCalls(t, paramSym, acc)

    // Support both nullary select and accidentally inserted empty-arg apply
    case Select(qual, "lowercase") => collectCalls(qual, paramSym, "lowercase" :: acc)
    case Select(qual, "uppercase") => collectCalls(qual, paramSym, "UPPERCASE" :: acc)
    case Apply(Select(qual, "lowercase"), Nil) => collectCalls(qual, paramSym, "lowercase" :: acc)
    case Apply(Select(qual, "uppercase"), Nil) => collectCalls(qual, paramSym, "UPPERCASE" :: acc)

    // Base case: reached the parameter identifier
    case ident: Ident if ident.symbol == paramSym => acc

    case other =>
      report.errorAndAbort(s"Unsupported function shape for FieldName => FieldName: ${other.show}")

  def fromLambda(term: Term): List[String] = term match
    case Inlined(_, _, lam) => fromLambda(lam)
    case Block(_, t) => fromLambda(t)
    case i: Ident =>
      i.symbol.tree match
        case v: ValDef =>
          v.rhs match
            case Some(rhs) => fromLambda(rhs)
            case None => report.errorAndAbort(s"Val '${v.name}' has no right-hand side")
        case d: DefDef =>
          d.rhs match
            case Some(rhs) => fromLambda(rhs)
            case None => report.errorAndAbort(s"Def '${d.name}' has no body")
        case _ => report.errorAndAbort(s"Identifier does not reference a lambda: ${i.show}")
    case Lambda(param: ValDef, body) => collectCalls(body, param.symbol, Nil)
    case Closure(meth, _) =>
      meth.symbol.tree match
        case DefDef(_, paramss, _, Some(body)) =>
          val termParams: List[ValDef] = paramss.collect { case TermParamClause(vps) => vps }.flatten
          termParams match
            case firstParam :: _ => collectCalls(body, firstParam.symbol, Nil)
            case Nil => report.errorAndAbort("Closure has no term parameters")
        case _ => report.errorAndAbort(s"Unsupported closure shape: ${meth.show}")
    case other => report.errorAndAbort(s"Expected a lambda of type FieldName => FieldName, but got: ${other.show}")

  val methods: List[String] = func.asTerm match
    case lam @ (Inlined(_, _, _) | Lambda(_, _) | Block(_, _)) => fromLambda(lam)
    case i: Ident =>
      i.symbol.tree match
        case v: ValDef =>
          v.rhs match
            case Some(rhs) => fromLambda(rhs)
            case None => report.errorAndAbort(s"Val '${v.name}' has no right-hand side")
        case _ => report.errorAndAbort(s"Identifier does not reference a val definition: ${i.show}")
    case other =>
      report.errorAndAbort(s"Expected a lambda or identifier of FieldName => FieldName, but got: ${other.show}")

  Expr(methods)
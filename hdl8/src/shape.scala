package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.quoted.*
import scala.NamedTuple


inline def showTypeOf[T]: String = ${ showTypeOfImpl[T] }
private def showTypeOfImpl[T: Type](using Quotes): Expr[String] =
  Expr(Type.show[T])


trait ShapeOf[T]:
  type Labels <: Tuple
  type Elems  <: Tuple
  val labels: Array[String]
  def getVT(t: T, name: String): ValueType

object ShapeOf:
  inline given auto[T]: ShapeOf[T] = derived[T]

  inline def derived[T]: ShapeOf[T] =
    println(s"calling derived")
    summonFrom {
      case m: Mirror.ProductOf[T] => product[T](using m)
      case _                      => structural[T]
    }

  private inline def labelsList[L <: Tuple]: List[String] =
    inline erasedValue[L] match
      case _: EmptyTuple => Nil
      case _: (h *: t)   => constValue[h].asInstanceOf[String] :: labelsList[t]


  inline def product[T](using m: Mirror.ProductOf[T]): ShapeOf[T] =
    println("ShapeOf: product")
    new ShapeOf[T]:
      type Labels = m.MirroredElemLabels
      type Elems  = m.MirroredElemTypes
      val labels: Array[String] = labelsList[m.MirroredElemLabels].toArray
      labels.foreach(x => print(s"${x}, "))
      def getVT(t: T, name: String): ValueType =
        val idx = labels.indexOf(name)
        t.asInstanceOf[Product].productElement(idx).asInstanceOf[ValueType]

  inline def structural[T](using scala.util.NotGiven[Mirror.ProductOf[T]]): ShapeOf[T] =
    ${ structuralImpl[T] }

  private def structuralImpl[T: Type](using Quotes): Expr[ShapeOf[T]] =
    println("ShapeOf: structuralImpl")
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val cls = tpe.typeSymbol
    cls.tree match
      case _: ClassDef => ()
      case _ => report.errorAndAbort(s"ShapeOf: ${tpe.show} is not a class; cannot derive structural shape")

    // Collect public, stable fields of subtype ValueType in declaration order
    val fieldSyms =
      cls.fieldMembers.filter { s =>
        val f = s.flags
        !f.is(Flags.Private) && !f.is(Flags.Mutable)
      }

    val fields: List[(String, TypeRepr)] =
      fieldSyms.flatMap { s =>
        s.tree match
          case vd: ValDef =>
            val t = vd.tpt.tpe.widen
            if t <:< TypeRepr.of[ValueType] then Some((s.name, t)) else None
          case _ => None
      }

    if fields.isEmpty then
      report.errorAndAbort(s"ShapeOf: no public stable ValueType vals found in ${tpe.show}")

    val dup = fields.groupBy(_._1).collect { case (n, xs) if xs.size > 1 => n }
    if dup.nonEmpty then
      report.errorAndAbort(s"ShapeOf: duplicate field names in ${tpe.show}: ${dup.mkString(",")}")

    // Build type members Labels/Elems
    val labelTs: TypeRepr =
      fields.foldRight(TypeRepr.of[EmptyTuple]) { case ((n, _), acc) =>
        AppliedType(TypeRepr.of[*:], List(ConstantType(StringConstant(n)), acc))
      }
    val elemTs: TypeRepr =
      fields.foldRight(TypeRepr.of[EmptyTuple]) { case ((_, t), acc) =>
        AppliedType(TypeRepr.of[*:], List(t, acc))
      }

    // Sanity: ensure they are tuples (instead of relying on <: in the pattern)
    if !(labelTs <:< TypeRepr.of[Tuple]) then
      report.errorAndAbort(s"internal: labels type is not a Tuple: ${labelTs.show}")
    if !(elemTs  <:< TypeRepr.of[Tuple]) then
      report.errorAndAbort(s"internal: elems type is not a Tuple: ${elemTs.show}")

    // Build labels array (no spread in quotes)
    val labelsArr: Expr[Array[String]] =
      '{ ${Expr.ofList(fields.map((n, _) => Expr(n)))}.toArray }

    (labelTs.asType, elemTs.asType) match
      case ('[l], '[e]) =>
        '{
          new ShapeOf[T]:
            type Labels = l
            type Elems  = e
            val labels: Array[String] = $labelsArr
            def getVT(t: T, name: String): ValueType =
              // Public `val a` generates a public no-arg method `a`
              val m = t.getClass.getMethod(name)
              m.invoke(t).asInstanceOf[ValueType]
        }
      case _ =>
        sys.error(s"No matching case found for (labelsT ${labelTs}, elemTs ${elemTs})")

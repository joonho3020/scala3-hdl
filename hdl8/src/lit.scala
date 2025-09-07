package hdl8

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.util.NotGiven
import scala.quoted.*

type HostTypeOf[T] = T match
  case UInt => Int
  case Bool => Boolean
  case _    => NamedTuple.Map[NTOf[T], [X] =>> HostTypeOf[X & ValueType]]

final class Lit[T](private val payload: Any) extends Selectable:
  type Fields = NamedTuple.Map[
    NTOf[T],
    [X] =>> Lit[X & ValueType]
  ]

  inline def selectDynamic(name: String): Any =
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray.asInstanceOf[Array[String]]
        val idx = labels.indexOf(name)
        if idx < 0 then
          throw new NoSuchElementException(s"${this.getClass.getName} has no field '$name'")
        val arr = payload.asInstanceOf[Array[Any]]
        new Lit[Any](arr(idx))
      case _ =>
        throw new NoSuchElementException(s"${summonInline[ValueOf[String]]}") // never reached, just keep total
    }
  transparent inline def get: HostTypeOf[T] =
    payload.asInstanceOf[HostTypeOf[T]]

object Lit:
  inline def of[T <: ValueType](inline lit: HostTypeOf[T]): Lit[T] =
    ${ ofImpl[T]('lit) }

  private def ofImpl[T: Type](lit: Expr[Any])(using Quotes): Expr[Lit[T]] =
    import quotes.reflect.*

    def mkArray(elems: List[Expr[Any]]): Expr[Array[Any]] =
      val arr = Varargs(elems)
      '{ Array[Any](${arr}: _*) }

    def buildFor(tpe: TypeRepr, v: Expr[Any]): Expr[Any] =
      v

    val payload: Expr[Any] = buildFor(TypeRepr.of[T], lit)
    '{ new Lit[T]($payload) }


package hdl9

import scala.deriving.*
import scala.compiletime.*
import scala.util.NotGiven
import scala.quoted.*
import scala.language.dynamics
import scala.NamedTuple.*

final class Reg[T](val t: T) extends Dynamic:
  type Fields = NamedTuple.Map[
    NamedTuple.From[T],
    [X] =>> Reg[X & ValueType]]

  transparent inline def selectDynamic(inline name: String): Reg[? <: ValueType] =
    ${ RegMacros.selectDynamicImpl[T]('t, 'name) }

  override def toString(): String =
    s"Reg(${t})"

object RegMacros:
  def selectDynamicImpl[T: Type](
    t: Expr[T], nameExpr: Expr[String]
  )(using Quotes): Expr[Reg[? <: ValueType]] =
    import quotes.reflect.*
    nameExpr.value match
      case Some(fieldName) =>
        val selected =
          try Select.unique(t.asTerm, fieldName)
          catch
            case _: Throwable =>
              report.errorAndAbort(s"${Type.show[T]} has no field '$fieldName'")

        val widenedFieldTpe = selected.tpe.widenTermRefByName
        if !(widenedFieldTpe <:< TypeRepr.of[ValueType]) then
          report.errorAndAbort(s"Field '$fieldName' type ${widenedFieldTpe.show} is not a subtype of ValueType")

        val selectedExpr = selected.asExpr
        widenedFieldTpe.asType match
          case '[ft] => '{ new Reg[ft & ValueType](${selectedExpr}.asInstanceOf[ft & ValueType]) }
      case None =>
        quotes.reflect.report.errorAndAbort("selectDynamic requires a literal field name")


final class Lit[T](private val payload: Any) extends Dynamic:
  inline def selectDynamic(name: String): Lit[?] =
    // NOTE: since selectables doesn't preserve the order,
    // even if we manage to generate HostTypeOf[T] using macros,
    // the order in which the fields are defined may change.
    summonFrom {
      case m: Mirror.ProductOf[T] =>
        val labels = constValueTuple[m.MirroredElemLabels].toArray
        val idx = labels.indexOf(name)
        val subpayload = payload.asInstanceOf[Product].productElement(idx)
        new Lit[Any](subpayload)
      case _ =>
        throw new NoSuchElementException(s"${summonInline[ValueOf[String]]}")
    }
  transparent inline def get: HostTypeOf[T] =
    payload.asInstanceOf[HostTypeOf[T]]

// Derive host-side representation types for HDL ValueTypes, including structural bundles
type HostTypeOf[T] = HostOf[T]#Out

trait HostOf[T]:
  type Out

object HostOf:
  given HostOf[UInt] with
    type Out = Int

  given HostOf[Bool] with
    type Out = Boolean

  given [A <: ValueType](using ha: HostOf[A]): HostOf[Vec[A]] with
    type Out = Seq[ha.Out]

  inline given [T <: Bundle]: HostOf[T] = ${ HostOfMacros.hostOfBundleImpl[T] }

// TODO: Implement macros that returns a NamedTuple for a given Bundle.
// Remember that bundles can be nested.
// Seems like we can use the following information to create macros for generating
// NamedTuples:

// Expansion
// Named tuples are in essence just a convenient syntax for regular tuples. In the internal representation, a named tuple type is represented at compile time as a pair of two tuples. One tuple contains the names as literal constant string types, the other contains the element types. The runtime representation of a named tuples consists of just the element values, whereas the names are forgotten. This is achieved by declaring NamedTuple in package scala as an opaque type as follows:

// opaque type NamedTuple[N <: Tuple, +V <: Tuple] >: V = V
// For instance, the Person type would be represented as the type

// NamedTuple[("name", "age"), (String, Int)]
// NamedTuple is an opaque type alias of its second, value parameter. The first parameter is a string constant type which determines the name of the element. Since the type is just an alias of its value part, names are erased at runtime, and named tuples and regular tuples have the same representation.

// A NamedTuple[N, V] type is publicly known to be a supertype (but not a subtype) of its value paramater V, which means that regular tuples can be assigned to named tuples but not vice versa.

// The NamedTuple object contains a number of extension methods for named tuples hat mirror the same functions in Tuple. Examples are apply, head, tail, take, drop, ++, map, or zip. Similar to Tuple, the NamedTuple object also contains types such as Elem, Head, Concat that describe the results of these extension methods.

// The translation of named tuples to instances of NamedTuple is fixed by the specification and therefore known to the programmer. This means that:

// All tuple operations also work with named tuples “out of the box”.
// Macro libraries can rely on this expansion.
object HostOfMacros:
  def hostOfBundleImpl[T <: ValueType: Type](using Quotes): Expr[HostOf[T]] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    if !(tpe <:< TypeRepr.of[Bundle]) then
      report.errorAndAbort(s"HostOfMacros.hostOfBundleImpl can only be used for Bundle, got: ${tpe.show}")

    def declaredValueTypeFieldsInOrder(sym: Symbol, tref: TypeRepr): List[(String, TypeRepr)] =
      sym.tree match
        case cdef: ClassDef =>
          cdef.body.collect { case v: ValDef =>
            val mt = tref.memberType(v.symbol)
            val ft = mt match
              case mt: MethodType => mt.resType
              case pt: PolyType => pt.resType
              case other => other
            if ft <:< TypeRepr.of[ValueType] then Some((v.name, ft)) else None
          }.flatten
        case _ =>
          val syms = sym.fieldMembers ++ sym.caseFields
          syms
            .distinctBy(_.name)
            .flatMap { s =>
              val mt = tref.memberType(s)
              val ft = mt match
                case mt: MethodType => mt.resType
                case pt: PolyType => pt.resType
                case other => other
              if ft <:< TypeRepr.of[ValueType] then Some((s.name, ft)) else None
            }

    val fields: List[(String, TypeRepr)] = declaredValueTypeFieldsInOrder(tpe.typeSymbol, tpe)

    def tupleOf(elems: List[TypeRepr]): TypeRepr =
      elems.foldRight(TypeRepr.of[EmptyTuple]) { (head, tail) =>
        (head.asType, tail.asType) match
          case ('[h], '[t]) => TypeRepr.of[h *: (t & Tuple)]
      }

    val nameTypes: List[TypeRepr] = fields.map { case (n, _) => ConstantType(StringConstant(n)) }

    val valueTypes: List[TypeRepr] = fields.map { case (_, ft) =>
      ft.asType match
        case '[f] => TypeRepr.of[HostTypeOf[f & ValueType]]
    }

    val namesTupleTpe = tupleOf(nameTypes)
    val valuesTupleTpe = tupleOf(valueTypes)

    val outTpe: TypeRepr = (namesTupleTpe.asType, valuesTupleTpe.asType) match
      case ('[n], '[v]) => TypeRepr.of[scala.NamedTuple.NamedTuple[n & Tuple, v & Tuple]]

    outTpe.asType match
      case '[o] => '{ new HostOf[T] { type Out = o } }


object BundleMacros:
  inline def printBundleFields[T <: Bundle]: Unit = ${ printBundleFieldsImpl[T] }

  private def printBundleFieldsImpl[T <: Bundle: Type](using Quotes): Expr[Unit] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]

    def declaredValueTypeFieldsInOrder(sym: Symbol, tref: TypeRepr): List[(String, TypeRepr)] =
      sym.tree match
        case cdef: ClassDef =>
          cdef.body.collect { case v: ValDef =>
            val mt = tref.memberType(v.symbol)
            val ft = mt match
              case mt: MethodType => mt.resType
              case pt: PolyType => pt.resType
              case other => other
            if ft <:< TypeRepr.of[ValueType] then Some((v.name, ft)) else None
          }.flatten
        case _ =>
          val syms = sym.fieldMembers ++ sym.caseFields
          syms
            .distinctBy(_.name)
            .flatMap { s =>
              val mt = tref.memberType(s)
              val ft = mt match
                case mt: MethodType => mt.resType
                case pt: PolyType => pt.resType
                case other => other
              if ft <:< TypeRepr.of[ValueType] then Some((s.name, ft)) else None
            }

    val fields: List[(String, TypeRepr)] = declaredValueTypeFieldsInOrder(tpe.typeSymbol, tpe)

    def tupleOf(elems: List[TypeRepr]): TypeRepr =
      elems.foldRight(TypeRepr.of[EmptyTuple]) { (head, tail) =>
        (head.asType, tail.asType) match
          case ('[h], '[t]) => TypeRepr.of[h *: (t & Tuple)]
      }

    val nameTypes: List[TypeRepr] = fields.map { case (n, _) => ConstantType(StringConstant(n)) }
    val namesTupleTpe = tupleOf(nameTypes)

    val msg: String = namesTupleTpe.asType match
      case '[nt] => s"${Type.show[T]}: field-names type = ${Type.show[nt & Tuple]}"

    '{ println(${Expr(msg)}) }

  inline def fieldNamesOf[T <: Bundle]: Tuple =
    ${ fieldNamesOfImpl[T] }

  private def fieldNamesOfImpl[T <: Bundle](using Quotes, Type[T]): Expr[Tuple] =
    import quotes.reflect.*

    val tpe = TypeRepr.of[T]
    val cls = tpe.typeSymbol

    def declaredValueTypeFieldsInOrder(sym: Symbol, tref: TypeRepr): List[(String, TypeRepr)] =
      sym.tree match
        case cdef: ClassDef =>
          cdef.body.collect { case v: ValDef =>
            val mt = tref.memberType(v.symbol)
            val ft = mt match
              case mt: MethodType => mt.resType
              case pt: PolyType => pt.resType
              case other => other
            if ft <:< TypeRepr.of[ValueType] then Some((v.name, ft)) else None
          }.flatten
        case _ =>
          val syms = sym.fieldMembers ++ sym.caseFields
          syms
            .distinctBy(_.name)
            .flatMap { s =>
              val mt = tref.memberType(s)
              val ft = mt match
                case mt: MethodType => mt.resType
                case pt: PolyType => pt.resType
                case other => other
              if ft <:< TypeRepr.of[ValueType] then Some((s.name, ft)) else None
            }

    val cls_fields: List[(String, TypeRepr)] = declaredValueTypeFieldsInOrder(tpe.typeSymbol, tpe)
    val fields: List[String] = cls_fields.map(_._1)

    val nameLits: List[Expr[String]] = fields.map(Expr(_))

    val namesTupleExpr: Expr[Tuple] =
    nameLits.foldRight('{ EmptyTuple }: Expr[Tuple]) { (h, acc) => '{ $h *: $acc } }

    namesTupleExpr

package hdl

private[hdl] object IR:
  opaque type Identifier = String
  object Identifier:
    def apply(value: String): Identifier = value
    extension (id: Identifier)
      def value: String = id

  enum PrimOp:
    case
      Add, Sub, Mul, Div, Rem,
      Lt, Leq, Gt, Geq, Eq, Neq,
      DShl, DShr,
      And, Or, Xor, Not,
      Cat, Pad, Shl, Shr, Head, Tail, Bits,
      Mux, AsUInt, AsBool

    def opName: String = this.productPrefix.toLowerCase()

  sealed trait Type extends Serializable
  final case class UIntType(width: Option[Width]) extends Type
  final case object BoolType extends Type
  final case object ClockType extends Type
  final case object ResetType extends Type
  final case class VecType(length: Int, elemType: Type) extends Type
  final case class BundleField(name: Identifier, flipped: Boolean, tpe: Type) extends Serializable
  final case class BundleType(fields: Seq[BundleField]) extends Type

  sealed trait Expr extends Serializable
  final case class Ref(name: Identifier) extends Expr
  final case class Literal(value: Identifier) extends Expr
  final case object DontCare extends Expr
  final case class DoPrim(op: PrimOp, args: Seq[Expr], consts: Seq[Int] = Seq.empty) extends Expr
  final case class SubIndex(expr: Expr, value: Int) extends Expr
  final case class SubAccess(expr: Expr, index: Expr) extends Expr
  final case class SubField(expr: Expr, field: Identifier) extends Expr

  sealed trait Stmt extends Serializable
  final case class Wire(name: Identifier, tpe: Type) extends Stmt
  final case class WireInit(name: Identifier, tpe: Type, clock: Expr, reset: Expr, init: Expr) extends Stmt
  final case class Reg(name: Identifier, tpe: Type, clock: Expr) extends Stmt
  final case class RegInit(name: Identifier, tpe: Type, clock: Expr, reset: Expr, init: Expr) extends Stmt
  final case class DefNode(name: Identifier, value: Expr) extends Stmt
  final case class Connect(loc: Expr, expr: Expr) extends Stmt
  final case class When(cond: Expr, conseq: Seq[Stmt], var alt: Seq[Stmt]) extends Stmt
  final case class Inst(name: Identifier, module: Identifier) extends Stmt
  final case class Invalid(expr: Expr) extends Stmt

  final case class Port(name: Identifier, direction: Direction, tpe: Type) extends Serializable
  final case class Module(name: Identifier, ports: Seq[Port], body: Seq[Stmt]) extends Serializable

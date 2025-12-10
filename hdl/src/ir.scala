package hdl

object IR:
  enum PrimOp:
    case Add, Eq, Neq

    def opName: String = this.productPrefix.toLowerCase()

  sealed trait Type extends Serializable
  final case class UIntType(width: Int) extends Type
  final case object BoolType extends Type
  final case class VecType(length: Int, elemType: Type) extends Type
  final case class BundleField(name: String, flipped: Boolean, tpe: Type) extends Serializable
  final case class BundleType(fields: Seq[BundleField]) extends Type

  sealed trait Expr extends Serializable
  final case class Ref(name: String) extends Expr
  final case class Literal(value: String) extends Expr
  final case class DoPrim(op: PrimOp, args: Seq[Expr]) extends Expr

  sealed trait Stmt extends Serializable
  final case class Wire(name: String, tpe: Type) extends Stmt
  final case class Reg(name: String, tpe: Type) extends Stmt
  final case class DefNode(name: String, value: Expr) extends Stmt
  final case class Connect(loc: Expr, expr: Expr) extends Stmt
  final case class When(cond: Expr, conseq: Seq[Stmt], var alt: Seq[Stmt]) extends Stmt
  final case class Inst(name: String, module: String) extends Stmt

  final case class Port(name: String, direction: Direction, tpe: Type) extends Serializable
  final case class Module(name: String, ports: Seq[Port], body: Seq[Stmt]) extends Serializable

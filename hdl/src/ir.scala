package hdl

sealed trait TypeIR

object TypeIR:
  case class UIntIR(width: Int) extends TypeIR
  case class BoolIR() extends TypeIR
  case class VecIR(elem: TypeIR, len: Int) extends TypeIR
  case class BundleIR(name: String, fields: Seq[(String, TypeIR)]) extends TypeIR

sealed trait ExprIR

object ExprIR:
  case class Ref(name: String) extends ExprIR
  case class Lit(value: Any) extends ExprIR
  case class SubField(target: ExprIR, field: String) extends ExprIR
  case class SubIndex(target: ExprIR, index: Int) extends ExprIR
  case class Mux(sel: ExprIR, tval: ExprIR, fval: ExprIR) extends ExprIR
// case class PrimOp(op: String, args: Seq[ExprIR]) extends ExprIR

sealed trait StmtIR

object StmtIR:
  case class WireDecl(name: String, tpe: TypeIR) extends StmtIR
  case class RegDecl(name: String, tpe: TypeIR) extends StmtIR
  case class Connect(lhs: ExprIR, rhs: ExprIR) extends StmtIR
  case class Instance(name: String, module: String) extends StmtIR
// case class When(cond: ExprIR, conseq: Seq[StmtIR], alt: Seq[StmtIR]) extends StmtIR

case class PortIR(name: String, tpe: TypeIR, dir: Direction)

case class ModuleIR(
  name: String,
  ports: Seq[PortIR],
  body: Seq[StmtIR]
)

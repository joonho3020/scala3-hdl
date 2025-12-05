package hdl

import scala.deriving.*
import scala.compiletime.*
import scala.NamedTuple
import scala.language.dynamics

enum NodeKind:
  case Reg, Wire, IO, PrimOp, Lit, Unset

object Reg:
  def apply[T <: HWData](t: T, name: Option[String] = None): T =
    t.setNodeKind(NodeKind.Reg)
    t

object Lit:
  def apply[T <: BundleIf](t: T)(payload: HostTypeOf[T]): Bundle[T] =
    var bundle = Bundle(t)
    bundle.setNodeKind(NodeKind.Lit)
    bundle.setLitVal(payload)
    bundle

  def apply[T <: HWData](t: T)(payload: HostTypeOf[T]): T =
    t.setNodeKind(NodeKind.Lit)
    t.setLitVal(payload)
    t

// def apply[T <: BundleIf](t: T, name: Option[String] = None)(payload: HostTypeOf[T]): T =
// t.setNodeKind(NodeKind.Lit)
// t.setLitVal(payload)
// t

// object Wire:
// def apply[T <: HWData](t: T, name: Option[String] = None): Node[T] =
// Node(t, NodeKind.Wire, name, None, name.getOrElse(""))

// object IO:
// def apply[T <: HWData](t: T, name: Option[String] = None): Node[T] =
// Node(t, NodeKind.IO, name, None, name.getOrElse(""))

// object PrimOp:
// def apply[T <: HWData](t: T, name: Option[String] = None): Node[T] =
// Node(t, NodeKind.PrimOp, name, None, name.getOrElse(""))

// object Lit:
// def apply[T <: HWData](t: T, name: Option[String] = None)(payload: HostTypeOf[T]): Node[T] =
// Node(t, NodeKind.Lit, name, Some(payload), name.getOrElse(""))

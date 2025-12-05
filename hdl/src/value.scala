package hdl

enum LeafKinds:
  case Reg, Wire, IO, PrimOp, Lit, Unset

sealed trait LeafValue:
  var kind: LeafKinds
  var dir: Direction
  var name: Option[String]
  var literal: Option[Any]
  def duplicate(
    kind: LeafKinds = kind,
    dir: Direction = dir,
    name: Option[String] = name,
    literal: Option[Any] = literal
  ): LeafValue

final class UInt(
  val w: Width,
  var dir: Direction = Direction.Out,
  var kind: LeafKinds = LeafKinds.Unset,
  var name: Option[String] = None,
  var literal: Option[Any] = None
) extends LeafValue:
  def duplicate(
    kind: LeafKinds = kind,
    dir: Direction = dir,
    name: Option[String] = name,
    literal: Option[Any] = literal
  ): UInt =
    new UInt(w, dir, kind, name, literal)

  override def toString(): String = s"UInt($w, $dir)"

object UInt:
  def apply(w: Width): UInt = new UInt(w)

final class Bool(
  var dir: Direction = Direction.Out,
  var kind: LeafKinds = LeafKinds.Unset,
  var name: Option[String] = None,
  var literal: Option[Any] = None
) extends LeafValue:
  def duplicate(
    kind: LeafKinds = kind,
    dir: Direction = dir,
    name: Option[String] = name,
    literal: Option[Any] = literal
  ): Bool =
    new Bool(dir, kind, name, literal)

  override def toString(): String = s"Bool($dir)"

object Bool:
  def apply(): Bool = new Bool

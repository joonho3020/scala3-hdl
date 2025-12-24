package hdl

enum Direction:
  case Default, Flipped

object Direction:
  inline def In: Direction = Direction.Flipped
  inline def Out: Direction = Direction.Default
  inline def flip(d: Direction): Direction = d match
    case Direction.Default => Direction.Flipped
    case Direction.Flipped => Direction.Default


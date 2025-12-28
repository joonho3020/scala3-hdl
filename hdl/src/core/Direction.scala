package hdl

/** Direction of a hardware value or port. */
enum Direction:
  case Default, Flipped

object Direction:
  /** Alias for an input direction. */
  inline def In: Direction = Direction.Flipped
  /** Alias for an output direction. */
  inline def Out: Direction = Direction.Default
  /** Flip the direction. */
  inline def flip(d: Direction): Direction = d match
    case Direction.Default => Direction.Flipped
    case Direction.Flipped => Direction.Default

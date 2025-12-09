package hdl

trait WalkHW[T]:
  def apply(x: T, path: String)(f: (HWData, String) => Unit): Unit

object WalkHW:
  private def walkAny(x: Any, path: String, f: (HWData, String) => Unit): Unit =
    x match
      case h: HWData =>
        f(h, path)
        x match
          case p: Product    => walkProduct(p, path, f)
          case it: IterableOnce[?] => walkIterable(it, path, f)
          case _ => ()
      case it: IterableOnce[?] =>
        walkIterable(it, path, f)
      case p: Product =>
        walkProduct(p, path, f)
      case _ => ()

  private def walkIterable(it: IterableOnce[?], path: String, f: (HWData, String) => Unit): Unit =
    val iter = it.iterator
    var idx = 0
    while iter.hasNext do
      val n = if path.isEmpty then s"[$idx]" else s"$path[$idx]"
      walkAny(iter.next(), n, f)
      idx += 1

  private def walkProduct(p: Product, path: String, f: (HWData, String) => Unit): Unit =
    val arity = p.productArity
    var i = 0
    while i < arity do
      val name =
        try p.productElementName(i)
        catch case _: Throwable => i.toString
      val childPath = if path.isEmpty then name else s"$path.$name"
      walkAny(p.productElement(i), childPath, f)
      i += 1

  given universal[T]: WalkHW[T] with
    def apply(x: T, path: String)(f: (HWData, String) => Unit): Unit =
      walkAny(x, path, f)

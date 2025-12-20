package hdl

private[hdl] object HWAggregate:
  def foreach[T](value: T, path: String = "")(f: (HWData, String) => Unit): Unit =
    visitAny(value, path, f)

  private def visitAny(x: Any, path: String, f: (HWData, String) => Unit): Unit =
    x match
      case h: HWData =>
        f(h, path)
        h match
          case v: Vec[?] => visitIterable(v.elems, path, f)
          case b: Bundle[?] => visitProduct(b.asInstanceOf[Product], path, f)
          case _ => ()
      case opt: Option[?] =>
        opt.foreach(elem => visitAny(elem, path, f))
      case it: Iterable[?] =>
        visitIterable(it, path, f)
      case p: Product =>
        visitProduct(p, path, f)
      case _ => ()

  private def visitIterable(
    it: Iterable[?],
    path: String,
    f: (HWData, String) => Unit
  ): Unit =
    val iter = it.iterator
    var idx = 0
    while iter.hasNext do
      val n = if path.isEmpty then s"[$idx]" else s"$path[$idx]"
      visitAny(iter.next(), n, f)
      idx += 1

  private def visitProduct(
    p: Product,
    path: String,
    f: (HWData, String) => Unit
  ): Unit =
    val arity = p.productArity
    var i = 0
    while i < arity do
      val name = productElementNameSafe(p, i)
      val childPath = if path.isEmpty then name else s"$path.$name"
      visitAny(p.productElement(i), childPath, f)
      i += 1

  private def transform[T](value: T, path: String = "", ctx: Option[IR.Expr] = None)(
    f: (HWData, String, HWData, Option[IR.Expr]) => HWData
  ): T =
    transformAny(value, path, ctx, f).asInstanceOf[T]

  private def transformAny(x: Any, path: String, ctx: Option[IR.Expr], f: (HWData, String, HWData, Option[IR.Expr]) => HWData): Any =
    x match
      case h: HWData =>
        transformHW(h, path, ctx, f)
      case opt: Option[?] =>
        opt.map(elem => transformAny(elem, path, ctx, f))
      case it: Iterable[?] =>
        transformIterable(it, path, ctx, f)
      case p: Product =>
        transformProduct(p, path, ctx, f)
      case other => other

  private def transformHW(h: HWData, path: String, ctx: Option[IR.Expr], f: (HWData, String, HWData, Option[IR.Expr]) => HWData): HWData =
    val rebuilt: HWData = h match
      case u: UInt =>
        val x = u.cloneType
        x
      case s: SInt =>
        val x = s.cloneType
        x
      case b: Bool =>
        Bool()
      case c: Clock =>
        Clock()
      case r: Reset =>
        Reset()
      case oh: OneHot =>
        oh.cloneType
      case e: HWEnum[?] =>
        e.cloneType
      case _: DontCare.type =>
        DontCare
      case v: Vec[?] =>
        val elems = v.elems.zipWithIndex.map { case (e, idx) =>
          transformAny(e, indexPath(path, idx), ctx.map(expr => IR.SubIndex(expr, idx)), f).asInstanceOf[HWData]
        }
        Vec(elems.asInstanceOf[Seq[HWData]])
      case b: Bundle[?] =>
        val prod = b.asInstanceOf[Product]
        val arity = prod.productArity
        val values = Array.ofDim[Any](arity)
        var i = 0
        while i < arity do
          val name = productElementNameSafe(prod, i)
          val childPath = if path.isEmpty then name else s"$path.$name"
          val childCtx = ctx.map(expr => IR.SubField(expr, IR.Identifier(name)))
          values(i) = transformAny(prod.productElement(i), childPath, childCtx, f)
          i += 1
        rebuildProduct(prod, values).asInstanceOf[HWData]
    f(h, path, rebuilt, ctx)

  private def transformIterable(it: Iterable[?], path: String, ctx: Option[IR.Expr], f: (HWData, String, HWData, Option[IR.Expr]) => HWData): Iterable[?] =
    val mapped = it.iterator.zipWithIndex.map { case (elem, idx) =>
      val childPath = indexPath(path, idx)
      val childCtx = ctx.map(expr => IR.SubIndex(expr, idx))
      transformAny(elem, childPath, childCtx, f)
    }.toSeq
    it.iterableFactory.from(mapped)

  private def transformProduct(p: Product, path: String, ctx: Option[IR.Expr], f: (HWData, String, HWData, Option[IR.Expr]) => HWData): Any =
    val arity = p.productArity
    val values = Array.ofDim[Any](arity)
    var i = 0
    while i < arity do
      val name = productElementNameSafe(p, i)
      val childPath = if path.isEmpty then name else s"$path.$name"
      values(i) = transformAny(p.productElement(i), childPath, ctx, f)
      i += 1
    rebuildProduct(p, values)

  def cloneData[T <: HWData](template: T): T =
    transform(template) { (orig, _, rebuilt, _) =>
      rebuilt.dir = orig.dir
      rebuilt.kind = NodeKind.Unset
      rebuilt.literal = orig.literal
      rebuilt
    }.asInstanceOf[T]

  def rebind[T <: HWData](template: T, expr: IR.Expr): T =
    transform(template, ctx = Some(expr)) { (orig, _, rebuilt, ctxOpt) =>
      rebuilt.dir = orig.dir
      rebuilt.kind = orig.kind
      rebuilt.literal = orig.literal
      orig.getOwner.foreach(rebuilt.setOwner)
      ctxOpt.foreach(rebuilt.setIRExpr)
      rebuilt
    }.asInstanceOf[T]

  private def rebuildProduct(p: Product, values: Array[Any]): Any =
    val cls = p.getClass
    val ctors = cls.getConstructors
    val args = values.map(_.asInstanceOf[AnyRef])
    val directCtor = ctors.collectFirst {
      case c if c.getParameterCount == args.length =>
        () => c.newInstance(args*)
    }
    val outerCtor = ctors.collectFirst {
      case c if c.getParameterCount == args.length + 1 =>
        cls.getDeclaredFields.find(_.getName == "$outer").map { outerField =>
          outerField.setAccessible(true)
          val outer = outerField.get(p)
          val outerArgs: Array[AnyRef] = Array(outer.asInstanceOf[AnyRef]) ++ args
          () => c.newInstance(outerArgs*)
        }
    }.flatten
    directCtor.orElse(outerCtor)
      .map(_())
      .getOrElse {
        val companionOpt = try Some(Class.forName(s"${cls.getName}$$")) catch
          case _: Throwable => None
        val module = companionOpt.map(_.getField("MODULE$").get(null))
          .getOrElse(throw new IllegalArgumentException(s"No apply method for ${cls.getName}"))
        val apply = module.getClass.getMethods.find(m => m.getName == "apply" && m.getParameterCount == values.length)
          .getOrElse(throw new IllegalArgumentException(s"No apply method for ${cls.getName}"))
        apply.invoke(module, args*)
      }

  private def productElementNameSafe(p: Product, idx: Int): String =
    try p.productElementName(idx)
    catch case _: Throwable => idx.toString

  private def indexPath(path: String, idx: Int): String =
    if path.isEmpty then s"[$idx]" else s"$path[$idx]"

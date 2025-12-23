package hdl

object WhenOps:
  def when(cond: Bool, mod: Module)(block: => Unit): WhenDSL =
    val condExpr = ModuleOps.exprFor(cond, mod)
    val conseq = mod.getBuilder.captureBody {
      block
    }
    val raw = RawWhen(condExpr, conseq, Seq.empty)
    mod.getBuilder.addRaw(raw)
    new WhenDSL(mod, raw)

final class WhenDSL(private val mod: Module, private val current: RawWhen):
  def elsewhen(cond: Bool)(block: => Unit): WhenDSL =
    val body = mod.getBuilder.captureBody {
      block
    }
    val nested = RawWhen(ModuleOps.exprFor(cond, mod), body, Seq.empty)
    current.alt = Seq(nested)
    new WhenDSL(mod, nested)

  def otherwise(block: => Unit): Unit =
    current.alt = mod.getBuilder.captureBody {
      block
    }

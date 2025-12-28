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

/** Builder returned from when for chaining elsewhen/otherwise. */
final class WhenDSL(private val mod: Module, private val current: RawWhen):
  /** Add a conditional branch to the current when chain. */
  def elsewhen(cond: Bool)(block: => Unit): WhenDSL =
    val body = mod.getBuilder.captureBody {
      block
    }
    val nested = RawWhen(ModuleOps.exprFor(cond, mod), body, Seq.empty)
    current.alt = Seq(nested)
    new WhenDSL(mod, nested)

  /** Add a default branch to the current when chain. */
  def otherwise(block: => Unit): Unit =
    current.alt = mod.getBuilder.captureBody {
      block
    }

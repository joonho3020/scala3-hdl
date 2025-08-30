package hdl.plugins

import dotty.tools.dotc.plugins._
import dotty.tools.dotc.ast.tpd
import dotty.tools.dotc.ast.tpd._
import dotty.tools.dotc.core._
import dotty.tools.dotc.core.Contexts._
import dotty.tools.dotc.core.Symbols._
import dotty.tools.dotc.core.Names._
import dotty.tools.dotc.core.StdNames._
import dotty.tools.dotc.core.Flags._
import dotty.tools.dotc.core.Types._

final class InjectPhase extends PluginPhase {
  override val phaseName  = "bundle-lit-inject"
  override val runsAfter  = Set("pickler")
  override val runsBefore = Set("erasure")

  override def transformPackageDef(tree: PackageDef)(using Context): Tree = {
    println(s"[bundle-lit] visiting ${tree.pid.show}")
    tree
  }
}

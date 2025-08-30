package hdl.plugins

import dotty.tools.dotc.plugins._
import dotty.tools.dotc.core.Contexts.*

class BundleLitPlugins extends StandardPlugin {
  val name = "bundlelit"
  val description = "Inject lit methods into Bundle companions"

  def init(options: List[String]): List[PluginPhase] =
    List(new InjectPhase)
}

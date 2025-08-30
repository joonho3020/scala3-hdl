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
import dotty.tools.dotc.ast.untpd.ModuleDef

final class InjectPhase extends PluginPhase {
  override val phaseName  = "bundle-lit-inject"
  override val runsAfter  = Set("pickler")
  override val runsBefore = Set("erasure")

  override def transformPackageDef(tree: PackageDef)(using Context): Tree = {
// println(s"[bundle-lit] visiting ${tree.pid.show}")

    // Collect bundle classes in this package
    val bundleOwners = scala.collection.mutable.HashSet[Symbol]()
    val bundleSymbol: ClassSymbol = requiredClass("hdl.Bundle")
    tree.foreachSubTree {
      case td: TypeDef if td.isClassDef =>
        val cls = td.symbol.asClass
        val clsName = cls.name
        val clsType = cls.typeRef

// println(s"cls: ${cls.show} ${clsName.show} ${clsType}")
        if cls.thisType.baseClasses.contains(bundleSymbol) && cls != bundleSymbol then
          bundleOwners += cls
      case _ =>
    }

    println(s"bundleOwners ${bundleOwners}")

// tree.stats.foreach(x => println(s"${x}"))

    val newStats = tree.stats.map(x => x)
    cpy.PackageDef(tree)(tree.pid, newStats)

// tree
  }
}

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

    // Resolve hdl.Bundle
    val bundleClass: ClassSymbol = requiredClass("hdl.Bundle")

    // Collect classes in this package that are subclasses of Bundle
    val bundleOwners = scala.collection.mutable.HashSet[ClassSymbol]()
    tree.foreachSubTree {
      case td: TypeDef if td.isClassDef =>
        val cls = td.symbol.asClass
        val isSubclass = cls.isSubClass(bundleClass)
        if isSubclass && cls != bundleClass then
          bundleOwners += cls
      case _ =>
    }

    println(s"bundleOwners ${bundleOwners}")

    // Rewrite companion modules for those classes: inject trivial lit() if missing
    val newStats = tree.stats.map {
      case td: TypeDef if td.isClassDef && td.symbol.is(ModuleClass) =>
        val modClass  = td.symbol.asClass
        val compClass = modClass.companionClass
        println(s"compClass ${compClass} ${compClass.exists}")
        if compClass.exists then
          val ownerCls  = compClass.asClass
          val isTarget  = bundleOwners.contains(ownerCls)
          val impl      = td.rhs.asInstanceOf[Template]
          val already   = impl.body.exists {
            case d: DefDef if d.name == termName("lit") => true
            case _ => false
          }
          println(s"  already: ${already} isTarget ${isTarget}")

          if isTarget && !already then
            val mSym: TermSymbol = newSymbol(
              owner = modClass,
              name  = termName("lit"),
              flags = Method,
              info  = MethodType(Nil)(_ => Nil, _ => bundleClass.typeRef)
            ).asTerm
            val ctor  = Select(New(TypeTree(bundleClass.typeRef)), nme.CONSTRUCTOR)
            val rhs   = Apply(ctor, Nil)
            val defdef = DefDef(mSym, _ => rhs)

            val impl2  = cpy.Template(impl)(body = impl.body :+ defdef)
            println(s"impl2 ${impl2} td.name ${td.name} td ${td} ${td.show}")
            cpy.TypeDef(td)(td.name, impl2)
          else td
        else td

      case other => other
    }

    cpy.PackageDef(tree)(tree.pid, newStats)
  }
}

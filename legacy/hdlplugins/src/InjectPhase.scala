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
import dotty.tools.dotc.core.Definitions._

final class InjectPhase extends PluginPhase {
  override val phaseName  = "bundleinject"
  override val runsAfter  = Set("typer")
  override val runsBefore = Set("bundleexamine")

  override def transformPackageDef(tree: PackageDef)(using Context): Tree = {
    println(s"[bundle-lit] visiting ${tree.pid.show}")

    val bundleClass: ClassSymbol = requiredClass("hdl.Bundle")
    val uintClass: ClassSymbol   = requiredClass("hdl.UInt")
    val uintLitClass: ClassSymbol = requiredClass("hdl.UIntLit")
    val signalClass: ClassSymbol = requiredClass("hdl.Signal")

    // Collect classes in this package that are subclasses of Bundle and their field infos
    val fieldsByOwner = scala.collection.mutable.HashMap.empty[ClassSymbol, List[(TermName, Type)]]

    tree.foreachSubTree {
      case td: TypeDef if td.isClassDef && !td.symbol.is(ModuleClass) =>
        val cls = td.symbol.asClass
        val isSubclass = cls.isSubClass(bundleClass)
        if isSubclass && cls != bundleClass then
          val tmpl = td.rhs match
            case t: Template => t
            case _ => EmptyTree
          val fields: List[(TermName, Type)] = tmpl match
            case t: Template =>
              t.body.collect {
                case v: ValDef =>
                  val name = v.name
                  val vtpe  = v.symbol.info.widen
                  (name, vtpe)
              }.filter { case (_, tpe) =>
                // Only keep fields that are Signals or Bundles
                tpe <:< signalClass.typeRef || tpe <:< bundleClass.typeRef
              }
            case _ => Nil
          fieldsByOwner.update(cls, fields)
      case _ =>
    }

    // Rewrite companion modules for those classes: inject typed transparent inline lit(...) if missing
    val newStats = tree.stats.map {
      case td: TypeDef if td.isClassDef && td.symbol.is(ModuleClass) =>
        val modClass  = td.symbol.asClass
        val compClass = modClass.companionClass
        if compClass.exists then
          val ownerCls  = compClass.asClass
          val isTarget  = ownerCls.isSubClass(bundleClass) && ownerCls != bundleClass
          val impl      = td.rhs.asInstanceOf[Template]
          val already   = impl.body.exists {
            case d: DefDef if d.name == termName("lit") => true
            case _ => false
          }

          if isTarget && !already then
            val ownerFields: List[(TermName, Type)] = fieldsByOwner.getOrElse(ownerCls, Nil)

            // Build method type: parameters per field, result Any (transparent inline will expose refined type)
            val paramNames: List[TermName] = ownerFields.map(_._1)
            val paramTypes: List[Type] = ownerFields.map { case (_, tpe) =>
              if tpe <:< uintClass.typeRef then uintLitClass.typeRef
              else if tpe <:< bundleClass.typeRef then bundleClass.typeRef
              else tpe
            }

            val mSym: TermSymbol = newSymbol(
              owner = modClass,
              name  = termName("lit"),
              flags = Method | Inline | Transparent | Synthetic,
              info  = MethodType(paramNames)(_ => paramTypes, _ => defn.AnyType)
            ).asTerm

            // Mark parameters as inline params
            mSym.paramSymss.foreach { params =>
              params.foreach { p => p.setFlag(InlineParam) }
            }

            val defdef = DefDef(mSym, { paramss =>
              // paramss has a single list for our MethodType
              val params = paramss.headOption.getOrElse(Nil).map(_.asInstanceOf[ValDef].symbol)

              // hdl.Bundle.lit[Owner]( ("a", a), ("b", b), ... )
              val bundleModule = requiredModule("hdl.Bundle")
              val litSel = Select(ref(bundleModule.termRef), termName("lit"))
              val typeApplied = TypeApply(litSel, List(TypeTree(ownerCls.typeRef)))

              val pairs: List[Tree] = ownerFields.zipWithIndex.map { case ((name, _), idx) =>
                val k = Literal(dotty.tools.dotc.core.Constants.Constant(name.toString))
                val vRef = ref(params(idx))
                // Force tuple element type to (String, Signal) to satisfy expected varargs
                val tupleModule = requiredModule("scala.Tuple2")
                val tupleApply = Select(ref(tupleModule.termRef), nme.apply)
                val tupleTapp  = TypeApply(tupleApply, List(TypeTree(defn.StringType), TypeTree(signalClass.typeRef)))
                Apply(tupleTapp, List(k, vRef))
              }

              Apply(typeApplied, pairs)
            })

            val impl2  = cpy.Template(impl)(body = impl.body :+ defdef)
            cpy.TypeDef(td)(td.name, impl2)
          else td
        else td

      case other => other
    }

    cpy.PackageDef(tree)(tree.pid, newStats)
  }
}

final class ExaminePhase extends PluginPhase {
  override val phaseName  = "bundleexamine"
  override val runsAfter  = Set("bundleinject")
  override val runsBefore = Set("pickler")

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
        println(s"cls: ${cls}")
        println(td.show)
        if isSubclass && cls != bundleClass then
          bundleOwners += cls
      case _ =>
    }
    tree
  }
}

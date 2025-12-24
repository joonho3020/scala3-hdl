package hdl

import scala.quoted.*

object MacroUtils:
  def findEnclosingValName(using Quotes): Option[String] =
    import quotes.reflect.*

    def isUserDefinedName(name: String): Boolean =
      !name.startsWith("<") &&
      !name.startsWith("$") &&
      !name.contains("$") &&
      name != "_"

    def loop(sym: Symbol): Option[String] =
      if sym.isNoSymbol then None
      else if sym.isValDef &&
              !sym.flags.is(Flags.Synthetic) &&
              !sym.flags.is(Flags.Artifact) &&
              isUserDefinedName(sym.name) then
        Some(sym.name)
      else loop(sym.owner)

    loop(Symbol.spliceOwner)

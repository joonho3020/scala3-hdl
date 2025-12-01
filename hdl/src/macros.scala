package hdl

import scala.quoted.*

object NameMacros:
  // Macro to create a Wire with auto-captured name from enclosing val
  inline def wireWithName[T <: ValueType](tpe: T)(using ctx: ElabContext): Wire[T] =
    ${ wireWithNameImpl('tpe, 'ctx) }

  private def wireWithNameImpl[T <: ValueType: Type](
    tpe: Expr[T],
    ctx: Expr[ElabContext]
  )(using Quotes): Expr[Wire[T]] =
    import quotes.reflect.*

    val name = findEnclosingValName()
    '{ $ctx.wire($tpe, ${Expr(name)}) }

  // Macro to create a Reg with auto-captured name
  inline def regWithName[T <: ValueType](tpe: T)(using ctx: ElabContext): Reg[T] =
    ${ regWithNameImpl('tpe, 'ctx) }

  private def regWithNameImpl[T <: ValueType: Type](
    tpe: Expr[T],
    ctx: Expr[ElabContext]
  )(using Quotes): Expr[Reg[T]] =
    import quotes.reflect.*

    val name = findEnclosingValName()
    '{ $ctx.reg($tpe, ${Expr(name)}) }

  inline def moduleWithName[M <: Module](mod: M)(using ctx: ElabContext): M =
    ${ moduleWithNameImpl('mod, 'ctx) }

  private def moduleWithNameImpl[M <: Module: Type](
    mod: Expr[M],
    ctx: Expr[ElabContext]
  )(using Quotes): Expr[M] =
    import quotes.reflect.*

    val name = findEnclosingValName()
    '{ $ctx.instantiate($mod, ${Expr(name)}) }

  // Helper to find the enclosing val's name
  private def findEnclosingValName()(using Quotes): String =
    import quotes.reflect.*

    def searchOwners(sym: Symbol): String =
      if sym.isNoSymbol then "unnamed"
      else if sym.isValDef then sym.name
      else searchOwners(sym.owner)

    searchOwners(Symbol.spliceOwner.owner)

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

  // Helper to find the enclosing val's name
  private def findEnclosingValName()(using Quotes): String =
    import quotes.reflect.*

    def searchOwners(sym: Symbol): String =
      if sym.isNoSymbol then "unnamed"
      else if sym.isValDef then sym.name
      else searchOwners(sym.owner)

    searchOwners(Symbol.spliceOwner.owner)

object InstanceMacros:
  /** Creates an Instance with typed IO accessors.
   *  The macro inspects the module type M to find all IO[T] fields
   *  and generates InstancePort[T] accessors with proper types.
   *
   *  Uses `transparent inline` so the compiler infers the refined return type
   *  that includes the `io` field with its proper type.
   */
  transparent inline def createInstance[M <: Module](child: M)(using ctx: ElabContext): Instance[M] =
    ${ createInstanceImpl[M]('child, 'ctx) }

  private def createInstanceImpl[M <: Module: Type](
    child: Expr[M],
    ctx: Expr[ElabContext]
  )(using Quotes): Expr[Instance[M]] =
    import quotes.reflect.*

    val moduleType = TypeRepr.of[M]

    report.info(s"Module type: ${moduleType.show}")

    def generateWithIO(ioFieldType: TypeRepr): Expr[Instance[M]] =
      ioFieldType match
        case AppliedType(_, List(innerType)) =>
          report.info(s"Inner type: ${innerType.show}")
          innerType.asType match
            case '[t] =>
              // Generate code that accesses the io field directly using Select
              val ioAccessExpr = Select.unique(child.asTerm, "io").asExprOf[IO[t & ValueType]]
              '{
                val childModule = $child
                val childIO = $ioAccessExpr
                val inst = $ctx.instantiate(childModule)
                new hdl.Instance[M](inst.name, inst.module, inst.moduleIR) {
                  val io: hdl.InstancePort[t & ValueType] =
                    new hdl.InstancePort[t & ValueType](
                      inst.name,
                      childIO.t,
                      childIO.name
                    )
                }
              }
        case other =>
          report.info(s"io field has unexpected type structure: ${other.show}")
          '{ $ctx.instantiate($child) }

    val ioSymbol = moduleType.typeSymbol.fieldMember("io")

    if ioSymbol.isNoSymbol then
      report.info(s"No 'io' field found via fieldMember, trying declaredField")
      // Try declaredField for local classes
      val declaredIo = moduleType.typeSymbol.declaredField("io")
      if declaredIo.isNoSymbol then
        report.info(s"No 'io' field found, using fallback")
        '{ $ctx.instantiate($child) }
      else
        val ioFieldType = moduleType.memberType(declaredIo)
        report.info(s"Found io via declaredField, type: ${ioFieldType.show}")
        generateWithIO(ioFieldType)
    else
      val ioFieldType = moduleType.memberType(ioSymbol)
      report.info(s"Found io via fieldMember, type: ${ioFieldType.show}")
      generateWithIO(ioFieldType)

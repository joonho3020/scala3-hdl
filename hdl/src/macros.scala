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
   */
  inline def createInstance[M <: Module](child: M)(using ctx: ElabContext): Instance[M] =
    ${ createInstanceImpl[M]('child, 'ctx) }

  private def createInstanceImpl[M <: Module: Type](
    child: Expr[M],
    ctx: Expr[ElabContext]
  )(using Quotes): Expr[Instance[M]] =
    import quotes.reflect.*

    val moduleType = TypeRepr.of[M]
    val ioType = TypeRepr.of[IO]

    // Find all val fields in M that have type IO[T] for some T
    val ioFields = moduleType.typeSymbol.fieldMembers.filter { field =>
      field.tree match
        case ValDef(_, tpt, _) =>
          val fieldType = tpt.tpe
          fieldType <:< ioType.appliedTo(TypeRepr.of[ValueType])
        case _ => false
    }

    // For each IO field, extract its inner type T
    case class IOFieldInfo(name: String, innerType: TypeRepr)

    val fieldInfos = ioFields.map { field =>
      val fieldType = moduleType.memberType(field)
      // Extract T from IO[T]
      val innerType = fieldType match
        case AppliedType(_, List(t)) => t
        case _ => report.errorAndAbort(s"Expected IO[T] type for field ${field.name}, got $fieldType")
      IOFieldInfo(field.name, innerType)
    }

    // Generate the Instance creation with typed IO accessors
    // We create an anonymous class that extends Instance[M] with the IO accessors
    if fieldInfos.isEmpty then
      // No IO fields, just create a plain Instance
      '{ $ctx.instantiate($child) }
    else
      // Create Instance with typed IO accessor(s)
      // For now, we handle the common case of a single 'io' field
      fieldInfos.find(_.name == "io") match
        case Some(IOFieldInfo(_, innerType)) =>
          innerType.asType match
            case '[t] =>
              '{
                val inst = $ctx.instantiate($child)
                new Instance[M](inst.name, inst.module, inst.moduleIR) {
                  val io: InstancePort[t & ValueType] =
                    new InstancePort[t & ValueType](
                      inst.name,
                      inst.module.asInstanceOf[{ val io: IO[t & ValueType] }].io.t,
                      inst.module.asInstanceOf[{ val io: IO[t & ValueType] }].io.name
                    )
                }
              }
        case None =>
          // No 'io' field, create plain Instance
          // User can access other IO fields via the apply method
          '{ $ctx.instantiate($child) }

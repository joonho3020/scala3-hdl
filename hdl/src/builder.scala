package hdl

object builder:
  inline def Wire[T <: ValueType](tpe: T)(using ctx: ElabContext): Node[T] =
    NameMacros.wireWithName(tpe)

  inline def Reg[T <: ValueType](tpe: T)(using ctx: ElabContext): Node[T] =
    NameMacros.regWithName(tpe)

  inline def Module[M <: hdl.Module](mod: M)(using ctx: ElabContext): M =
    NameMacros.moduleWithName(mod)

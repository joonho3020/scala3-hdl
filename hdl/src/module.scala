package hdl

trait Module:
  def moduleName: String = getClass.getSimpleName.stripSuffix("$")

  def body(using ctx: ElabContext): Unit

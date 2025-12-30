package hdl.elaboration

import hdl.core._
import java.io.{File, FileWriter}
import java.nio.file.{Files, Path, Paths}

/**
 * Exports IO bundle schemas from elaborated designs for use in simulator codegen.
 *
 * The schema is exported as JSON containing the IR.Type information
 * from the top module's IO ports, which can be parsed by Rust build scripts
 * for generating type-safe peek/poke APIs.
 */
object IOSchemaExporter:

  import ujson._

  /**
   * Convert IR.Width to JSON
   */
  private def widthToJson(w: Width): Value =
    if w.known then Num(w.get) else Null

  /**
   * Convert IR.Type to JSON
   */
  private def typeToJson(tpe: IR.Type): Value =
    tpe match
      case IR.UIntType(w) =>
        Obj("type" -> "UInt", "width" -> widthToJson(w))
      case IR.SIntType(w) =>
        Obj("type" -> "SInt", "width" -> widthToJson(w))
      case IR.BoolType =>
        Obj("type" -> "Bool")
      case IR.ClockType =>
        Obj("type" -> "Clock")
      case IR.ResetType =>
        Obj("type" -> "Reset")
      case IR.OneHotType(w) =>
        Obj("type" -> "OneHot", "width" -> widthToJson(w))
      case IR.VecType(length, elemType) =>
        Obj("type" -> "Vec", "length" -> Num(length), "elem_type" -> typeToJson(elemType))
      case IR.BundleType(fields) =>
        val fieldArr = Arr(fields.map { f =>
          Obj(
            "name" -> Str(f.name.value),
            "flipped" -> Bool(f.flipped),
            "type" -> typeToJson(f.tpe)
          )
        }*)
        Obj("type" -> "Bundle", "fields" -> fieldArr)

  /**
   * Convert IR.Port to JSON
   */
  private def portToJson(port: IR.Port): Value =
    Obj(
      "name" -> Str(port.name.value),
      "direction" -> Str(if port.direction == Direction.In then "input" else "output"),
      "type" -> typeToJson(port.tpe)
    )

  /**
   * Export IO schema from the top-level elaborated design.
   *
   * @param designs All elaborated designs (top + children)
   * @param topName Name of the top module
   * @param outputPath Path where schema should be written (default: sim/test-outputs/io_schema.json)
   */
  def exportIOSchema(
    designs: Seq[ElaboratedDesign],
    topName: String,
    outputPath: String = "sim/test-outputs/io_schema.json"
  ): Unit =
    // Find the top module
    designs.find(_.name.value == topName) match
      case Some(topDesign) =>
        val schema = Obj(
          "module_name" -> Str(topDesign.name.value),
          "ports" -> Arr(topDesign.ir.ports.map(portToJson)*)
        )

        // Ensure output directory exists
        val path = Paths.get(outputPath)
        val parent = path.getParent
        if parent != null && !Files.exists(parent) then
          Files.createDirectories(parent)

        // Write JSON to file
        val writer = new FileWriter(outputPath)
        try
          ujson.writeTo(schema, writer, indent = 2)
          println(s"Exported IO schema to $outputPath")
        finally
          writer.close()

      case None =>
        println(s"Warning: Could not find top module '$topName' in elaborated designs")

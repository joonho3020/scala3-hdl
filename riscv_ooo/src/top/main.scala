package riscv_ooo

import hdl.core._
import hdl.util._
import hdl.elaboration._

import hdl.core._
import hdl.util._
import hdl.elaboration._
import java.io.{File, PrintWriter}
import scala.sys.process.*

def writeChirrtl(filename: String, content: String): Unit =
  val dir = new File("sim/test-outputs/chirrtl")
  dir.mkdirs()
  val pw = new PrintWriter(new File(dir, filename))
  pw.write(content)
  pw.close()

def runFirtool(firFile: String): (Int, String) =
  val cmd = Seq(
    "firtool",
    "--format=fir",
    "--verify-each=true",
    "--split-verilog",
    "-o", "sim/test-outputs/verilog",
    s"sim/test-outputs/chirrtl/$firFile"
  )
  val output = new StringBuilder
  val exitCode = cmd.!(ProcessLogger(s => output.append(s + "\n"), s => output.append(s + "\n")))
  (exitCode, output.toString)

@main def main(): Unit =
  val p = CoreParams(
    magic_mem_outstanding = 4,
    debug = true,
    pcBits = 64,
    xlenBits = 64,
    paddrBits = 64,
    fetchWidth = 2,
    intIssueWidth = 2,
    lsuIssueWidth = 2,
    icacheFetchBytes = 2 * 4,
    instBytes = 4,
    ic = ICacheParams(
      nSets = 8,
      nWays = 4,
      cacheLineBytes = 64),
    dc = DCacheParams(
      nSets = 8,
      nWays = 4,
      cacheLineBytes = 64,
      mshrs = 2),
    aluPipes = 2,
    lsu = LSUParams(),
    prf = PRFParams(numEntries = 64),
    br = BranchParams(inFlightBranches = 4)
  )

  val top = new Tile(p)

  val elaborator = new Elaborator
  val (designs, elaborationNs) = Timing.timeNs(elaborator.elaborate(top))
  val top_name = top.moduleName
  val top_name_hashed = designs.map(_.name.value).filter(_.contains(top_name)).head

  val chirrtl = elaborator.emitChirrtl(designs, top_name_hashed)
  val fir_filename = s"${top_name}.fir"

  println(s"Elaboration Finished (${Timing.formatNs(elaborationNs)})")

  // Export IO schema for simulator codegen
  IOSchemaExporter.exportIOSchema(designs, top_name_hashed)

  writeChirrtl(fir_filename, chirrtl)
  println("Wrote CHIRRTL File")

  val ((exitCode, output), verilogGenNs) = Timing.timeNs(runFirtool(fir_filename))
  println(s"Verilog Generation Finished (${Timing.formatNs(verilogGenNs)})")
  println(output)

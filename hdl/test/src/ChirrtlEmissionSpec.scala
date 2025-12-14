package hdl

import utest.*
import java.io.{File, PrintWriter}
import scala.sys.process.*

case class AdderIO(a: UInt, b: UInt, c: UInt) extends Bundle[AdderIO]

class Adder(length: Int) extends Module:
  given Module = this
  val io = IO(AdderIO(
    Input(UInt(Width(length))),
    Input(UInt(Width(length))),
    Output(UInt(Width(length)))
  ))
  val reg = Reg(UInt(Width(length)))
  reg := io.a + io.b
  io.c := reg

case class GCDIO(
  value1: UInt,
  value2: UInt,
  loadingValues: Bool,
  outputGCD: UInt,
  outputValid: Bool
) extends Bundle[GCDIO]

class GCD extends Module:
  given Module = this
  val io = IO(GCDIO(
    Input(UInt(Width(16))),
    Input(UInt(Width(16))),
    Input(Bool()),
    Output(UInt(Width(16))),
    Output(Bool())
  ))

  val x = Reg(UInt(Width(16)))
  val y = Reg(UInt(Width(16)))

  when(x > y) {
    x := x - y
  }.otherwise {
    y := y - x
  }

  when(io.loadingValues) {
    x := io.value1
    y := io.value2
  }

  io.outputGCD := x
  io.outputValid := y === 0.U

object ChirrtlEmissionSpec extends TestSuite:
  def writeChirrtl(filename: String, content: String): Unit =
    val dir = new File("test-outputs/chirrtl")
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
      "-o", "test-outputs/verilog",
      s"test-outputs/chirrtl/$firFile"
    )
    val output = new StringBuilder
    val exitCode = cmd.!(ProcessLogger(s => output.append(s + "\n"), s => output.append(s + "\n")))
    (exitCode, output.toString)

  val tests = Tests {
    test("Adder CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val adder = new Adder(2)
      val designs = elaborator.elaborate(adder)
      val chirrtl = elaborator.emitChirrtl(designs, "Adder")
      println("=" * 50)
      println("Adder CHIRRTL:")
      println(chirrtl)
      writeChirrtl("Adder.fir", chirrtl)
      assert(chirrtl.contains("FIRRTL version 3.3.0"))
      assert(chirrtl.contains("circuit Adder :"))
      assert(chirrtl.contains("module Adder :"))
    }

    test("GCD CHIRRTL emission") {
      val elaborator = new Elaborator(log = _ => ())
      val gcd = new GCD
      val designs = elaborator.elaborate(gcd)
      val chirrtl = elaborator.emitChirrtl(designs, "GCD")
      println("=" * 50)
      println("GCD CHIRRTL:")
      println(chirrtl)
      writeChirrtl("GCD.fir", chirrtl)
      assert(chirrtl.contains("FIRRTL version 3.3.0"))
      assert(chirrtl.contains("circuit GCD :"))
      assert(chirrtl.contains("when"))
      assert(chirrtl.contains("else :"))
    }

    test("Adder firtool validation") {
      val elaborator = new Elaborator(log = _ => ())
      val adder = new Adder(2)
      val designs = elaborator.elaborate(adder)
      val chirrtl = elaborator.emitChirrtl(designs, "Adder")
      writeChirrtl("Adder.fir", chirrtl)
      val (exitCode, output) = runFirtool("Adder.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }

    test("GCD firtool validation") {
      val elaborator = new Elaborator(log = _ => ())
      val gcd = new GCD
      val designs = elaborator.elaborate(gcd)
      val chirrtl = elaborator.emitChirrtl(designs, "GCD")
      writeChirrtl("GCD.fir", chirrtl)
      val (exitCode, output) = runFirtool("GCD.fir")
      println("firtool output:")
      println(output)
      if exitCode != 0 then throw new java.lang.AssertionError(s"firtool failed with exit code $exitCode: $output")
    }
  }
package hdl

import hdl.core._
import hdl.util._
import hdl.elaboration._

import utest.*

private def id(name: String): IR.Identifier = IR.Identifier(name)
private def portOut(name: String, tpe: IR.Type): IR.Port = IR.Port(id(name), Direction.Out, tpe)
private val clockPort: IR.Port = IR.Port(id("clock"), Direction.In, IR.ClockType)
private val resetPort: IR.Port = IR.Port(id("reset"), Direction.In, IR.ResetType)
private def u(width: Int): IR.Type = IR.UIntType(Width(width))
private def bf(name: String, flipped: Boolean, tpe: IR.Type): IR.BundleField = IR.BundleField(id(name), flipped, tpe)

def arbiter_iotype_check(): Unit =
  val elaborator = new Elaborator(log = _ => ())
  val designs = elaborator.elaborate(new Arbiter(UInt(8.W), 2))
  val arb = designs.map(_.ir).find(_.name.value.startsWith("Arbiter")).get

  val decoupledU8 = IR.BundleType(Seq(
    bf("valid", flipped = false, IR.BoolType),
    bf("ready", flipped = true, IR.BoolType),
    bf("bits", flipped = false, u(8))
  ))

  val expectedIo = IR.BundleType(Seq(
    bf("in", flipped = true, IR.VecType(2, decoupledU8)),
    bf("out", flipped = false, decoupledU8),
    bf("chosen", flipped = false, u(1))
  ))

  val expected = Seq(clockPort, resetPort, portOut("io", expectedIo))
  assert(arb.ports == expected)

def rrarbiter_iotype_check(): Unit =
  val elaborator = new Elaborator(log = _ => ())
  val designs = elaborator.elaborate(new RRArbiter(UInt(16.W), 4))
  val arb = designs.map(_.ir).find(_.name.value.startsWith("RRArbiter")).get

  val decoupledU16 = IR.BundleType(Seq(
    bf("valid", flipped = false, IR.BoolType),
    bf("ready", flipped = true, IR.BoolType),
    bf("bits", flipped = false, u(16))
  ))

  val expectedIo = IR.BundleType(Seq(
    bf("in", flipped = true, IR.VecType(4, decoupledU16)),
    bf("out", flipped = false, decoupledU16),
    bf("chosen", flipped = false, u(2))
  ))

  val expected = Seq(clockPort, resetPort, portOut("io", expectedIo))
  assert(arb.ports == expected)

object ArbiterSpec extends TestSuite:
  val tests = Tests {
    test("arbiter_iotype_check") { arbiter_iotype_check() }
    test("rrarbiter_iotype_check") { rrarbiter_iotype_check() }
  }

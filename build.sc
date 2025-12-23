import mill._
import mill.scalalib._

val SCALAVERSION = "3.7.0"

object hdl extends ScalaModule {
  def scalaVersion = SCALAVERSION

  object test extends ScalaTests {
    def mvnDeps = Seq(mvn"com.lihaoyi::utest:0.9.1")
    def testFramework = "utest.runner.Framework"
  }
}

object riscv extends ScalaModule {
  def scalaVersion = SCALAVERSION
  override def moduleDeps = Seq(hdl)
}

object riscv_ooo extends ScalaModule {
  def scalaVersion = SCALAVERSION
  override def moduleDeps = Seq(hdl, riscv)
}

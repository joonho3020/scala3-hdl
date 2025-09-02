import mill._, scalalib._

val SCALAVERSION = "3.7.0"

object hdlplugins extends ScalaModule {
  def scalaVersion = SCALAVERSION
  def mvnDeps = Seq(
    mvn"org.scala-lang:scala3-compiler_3:3.3.1"
  )
}

object hdl extends ScalaModule {
  def scalaVersion = SCALAVERSION
  override def moduleDeps = super.moduleDeps ++ Seq(hdlplugins)
  override def scalacOptions = Task {
    val pluginJar = hdlplugins.jar().path
    println(s"pluginJar ${pluginJar}")
    super.scalacOptions() ++ Seq(s"-Xplugin:${pluginJar}")
  }
}

// object playground extends ScalaModule {
// def scalaVersion = SCALAVERSION
// override def moduleDeps = Seq(hdl)
// }

object playground2 extends ScalaModule {
  def scalaVersion = "3.7.0"
  override def moduleDeps = Seq(hdl)
}

object hdl2 extends ScalaModule {
  def scalaVersion = SCALAVERSION
}

object hdl2_playground extends ScalaModule {
  def scalaVersion = SCALAVERSION
  override def moduleDeps = super.moduleDeps ++ Seq(hdl2)
}

object dynamic extends ScalaModule {
  def scalaVersion = "3.7.0"
  override def moduleDeps = Seq(hdl)
}

object hdl3 extends ScalaModule {
  def scalaVersion = SCALAVERSION
}

object hdl4 extends ScalaModule {
  def scalaVersion = SCALAVERSION
}

object hdl5 extends ScalaModule {
  def scalaVersion = SCALAVERSION
}

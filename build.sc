import mill._, scalalib._

object hdlplugins extends ScalaModule {
  def scalaVersion = "3.3.1"
  def mvnDeps = Seq(
    mvn"org.scala-lang:scala3-compiler_3:3.3.1"
  )
}

object hdl extends ScalaModule {
  def scalaVersion = "3.3.1"
  override def moduleDeps = super.moduleDeps ++ Seq(hdlplugins)
  override def scalacOptions = Task {
    val pluginJar = hdlplugins.jar().path
    println(s"pluginJar ${pluginJar}")
    super.scalacOptions() ++ Seq(s"-Xplugin:${pluginJar}")
  }
}

object playground extends ScalaModule {
  def scalaVersion = "3.3.1"
}

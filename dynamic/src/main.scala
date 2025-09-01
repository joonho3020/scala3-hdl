import scala.language.dynamics

final class Loggy(private val path: List[String] = Nil) extends Dynamic:
  private def here(next: String) = new Loggy(path :+ next)
  private def show = if path.isEmpty then "<root>" else path.mkString(".")

  def selectDynamic(name: String): Loggy =
    println(s"selectDynamic('$name') at $show")
    here(name)

object Main:
  def main(args: Array[String]): Unit =
    val d = Loggy()
    d.foo

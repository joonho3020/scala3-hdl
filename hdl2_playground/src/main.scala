package hdl2_playground

import hdl2._

object Main:
  def main(args: Array[String]): Unit =
    println("Hello World")

    val width1 = Width(1)
    val width2 = Width(2)
    println(s"${width1.show} ${width2.show}")

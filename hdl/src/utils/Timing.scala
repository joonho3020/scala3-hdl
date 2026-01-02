package hdl.util

object Timing:
  def timeNs[A](thunk: => A): (A, Long) =
    val start = System.nanoTime()
    val value = thunk
    val end = System.nanoTime()
    (value, end - start)

  def formatNs(ns: Long): String =
    val seconds = ns.toDouble / 1e9
    f"$seconds%.3f s"

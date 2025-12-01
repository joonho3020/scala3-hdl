// From:
// https://github.com/kitlangton/quotidian/blob/2c1ffb497bbc91f8860a965e757812d6609ff84c/examples/src/main/scala/quotidian/examples/lens/Lens.scala
package quotidian.examples.lens

trait Lens[S, A]:
  def get(s: S): A
  def set(s: S, a: A): S

  def modify(s: S)(f: A => A): S =
    set(s, f(get(s)))

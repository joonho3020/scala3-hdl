package hdl

import scala.annotation.experimental

trait BundleLit[B]

@experimental
object BundleLit:
  inline def derived[B]: BundleLit[B] = ${ BundleLitImpl.derivedImpl[B] }

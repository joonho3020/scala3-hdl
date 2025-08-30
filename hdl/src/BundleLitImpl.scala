package hdl

import scala.quoted.*
import scala.annotation.experimental


@experimental
object BundleLitImpl:
  def derivedImpl[B: Type](using Quotes): Expr[BundleLit[B]] =
    import quotes.reflect.*

    Type.of[B] match
      case '[t] =>
        '{
          new BundleLit[t]:
            extension (comp: t)
              def lit(): Bundle =
                new Bundle()
        }.asExprOf[BundleLit[B]]

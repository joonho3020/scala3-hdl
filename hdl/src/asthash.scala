package hdl

import scala.quoted.*
import java.io.{ObjectInputStream, ObjectOutputStream}
import java.nio.file.{Files, Path, Paths}
import java.security.MessageDigest

object ModuleCodeHash:
  inline def astHash[T]: String = ${ astHashImpl[T] }

  private def astHashImpl[T: Type](using Quotes): Expr[String] =

    import quotes.reflect.*

    def hashTree(tree: Tree, md: MessageDigest): Unit =
      def update(s: String): Unit =
        md.update(s.getBytes("UTF-8"))

      tree match
        case ClassDef(name, _, parents, body, _) =>
          update("ClassDef"); update(name)
          parents.foreach(hashTree(_, md))
          body.foreach(hashTree(_, md))

        case DefDef(name, _, _, rhsOpt) =>
          update("DefDef"); update(name)
          rhsOpt.foreach(hashTree(_, md))

        case ValDef(name, tpt, rhsOpt) =>
          update("ValDef"); update(name)
          hashTree(tpt, md)
          rhsOpt.foreach(hashTree(_, md))

        case Apply(fun, args) =>
          update("Apply")
          hashTree(fun, md)
          args.foreach(hashTree(_, md))

        case Select(qual, name) =>
          update("Select"); update(name)
          hashTree(qual, md)

        case Literal(const) =>
          update("Literal"); update(const.show)

        case Block(stats, expr) =>
          update("Block")
          stats.foreach(hashTree(_, md))
          hashTree(expr, md)

        case Inlined(_, _, expr) =>
          hashTree(expr, md)

        case t: TypeTree =>
          update("TypeTree"); update(t.tpe.show)

        case other =>
          // Fallback: use node kind and recurse on children, but *never* positions
          update(other.getClass.getSimpleName)
          hashTree(other, md)

    val sym  = TypeRepr.of[T].typeSymbol
    val tree = sym.tree

    val md = MessageDigest.getInstance("SHA-256")
    hashTree(tree, md)
    val hex = md.digest().map("%02x".format(_)).mkString
    Expr(hex)

package hdl.elaboration

import hdl.core._

import java.nio.charset.StandardCharsets
import java.security.MessageDigest
import scala.compiletime.{erasedValue, summonInline}
import scala.deriving.Mirror

trait StableHash[-T]:
  def hashInto(d: MessageDigest, x: T): Unit

object StableHash:
  def apply[T](using h: StableHash[T]): StableHash[T] = h

  def hashInto[T](d: MessageDigest, x: T)(using h: StableHash[T]): Unit =
    h.hashInto(d, x)

  def digest[T](x: T)(using h: StableHash[T]): Array[Byte] =
    val md = MessageDigest.getInstance("SHA-256")
    h.hashInto(md, x)
    md.digest()

  def hex(bytes: Array[Byte]): String =
    val sb = new StringBuilder(bytes.length * 2)
    var i = 0
    while i < bytes.length do
      val b = bytes(i) & 0xff
      if b < 16 then sb.append('0')
      sb.append(Integer.toHexString(b))
      i += 1
    sb.toString

  private def putInt(md: MessageDigest, i: Int): Unit =
    md.update(((i >>> 24) & 0xff).toByte)
    md.update(((i >>> 16) & 0xff).toByte)
    md.update(((i >>> 8) & 0xff).toByte)
    md.update((i & 0xff).toByte)

  given StableHash[Unit] with
    def hashInto(d: MessageDigest, x: Unit): Unit =
      d.update(0.toByte)

  given StableHash[Boolean] with
    def hashInto(d: MessageDigest, x: Boolean): Unit =
      d.update(if x then 1.toByte else 0.toByte)

  given StableHash[Byte] with
    def hashInto(d: MessageDigest, x: Byte): Unit =
      d.update(x)

  given StableHash[Short] with
    def hashInto(d: MessageDigest, x: Short): Unit =
      putInt(d, x.toInt)

  given StableHash[Int] with
    def hashInto(d: MessageDigest, x: Int): Unit =
      putInt(d, x)

  given StableHash[Long] with
    def hashInto(d: MessageDigest, x: Long): Unit =
      putInt(d, (x >>> 32).toInt)
      putInt(d, x.toInt)

  given StableHash[String] with
    def hashInto(d: MessageDigest, x: String): Unit =
      val bytes = x.getBytes(StandardCharsets.UTF_8)
      putInt(d, bytes.length)
      d.update(bytes)

  given StableHash[BigInt] with
    def hashInto(d: MessageDigest, x: BigInt): Unit =
      val bytes = x.toByteArray
      putInt(d, bytes.length)
      d.update(bytes)

  given [A](using StableHash[A]): StableHash[Option[A]] with
    def hashInto(d: MessageDigest, x: Option[A]): Unit =
      x match
        case Some(v) =>
          d.update(1.toByte)
          summon[StableHash[A]].hashInto(d, v)
        case None =>
          d.update(0.toByte)

  given [A](using StableHash[A]): StableHash[Seq[A]] with
    def hashInto(d: MessageDigest, x: Seq[A]): Unit =
      putInt(d, x.length)
      var i = 0
      while i < x.length do
        summon[StableHash[A]].hashInto(d, x(i))
        i += 1

  inline given derived[T](using m: Mirror.ProductOf[T]): StableHash[T] =
    val elemHashes: List[StableHash[Any]] = summonAll[m.MirroredElemTypes]
    new StableHash[T]:
      def hashInto(d: MessageDigest, x: T): Unit =
        val p = x.asInstanceOf[Product]
        var i = 0
        while i < elemHashes.length do
          elemHashes(i).hashInto(d, p.productElement(i))
          i += 1

  private inline def summonAll[T <: Tuple]: List[StableHash[Any]] =
    inline erasedValue[T] match
      case _: EmptyTuple => Nil
      case _: (h *: t) => summonInline[StableHash[h]].asInstanceOf[StableHash[Any]] :: summonAll[t]

package com.boldradius.total

sealed trait Key[K] {
  def total[V](f: K => V) : Total[K, V]
  def remove(k : K) : KeyContraction[K]
  def toStream : Stream[K]
}
case object NoKey extends Key[Nothing] {
  def total[V](f: Nothing => V) = TotalNothing
  def remove(k : Nothing) = k
  def toStream = Stream.empty
}
case object UnitKey extends Key[Unit] {
  def total[V](f: Unit => V) = TotalUnit(f())
  def remove(k : Unit) = new KeyContraction[Unit] {
    override def contract(a1: Unit): Option[Type] = None
    override def key: Key[Type] = NoKey
    override type Type = Nothing
    override val removed = ()
    override val fun: Fun[Either[Type, Unit], Unit] = TwoFun[Nothing, Unit, Unit](IdFun(), IdFun())
  }
  def toStream = Stream(())
}
case class EitherKey[A, B](a: Key[A], b: Key[B]) extends Key[Either[A, B]] {
  def total[V](f: Either[A, B] => V) = TotalEither(a.total(f.compose(Left(_))), b.total(f.compose(Right(_)))) // TODO FIXME f get slower and slower as we recurse, so we are O(n log n) instead of O(n)
  def remove(k : Either[A, B]) = k.fold(av => {
    val aContraction = a.remove(av)
    new KeyContraction[Either[A, B]] {
      def contract(a1: Either[A, B]): Option[Type] = a1.fold(aContraction.contract(_).map(Left(_)), b1 => Some(Right(b1)))
      def key: Key[Type] = EitherKey(aContraction.key, b)
      type Type = Either[aContraction.Type, B]
      val removed: Either[A, B] = k
      val fun: Fun[Either[Type, Unit], Either[A, B]] = Fun.leftMerge(aContraction.fun)
    }}, bv => {
      val bContraction = b.remove(bv)
      new KeyContraction[Either[A, B]] {
        def key: Key[Type] = EitherKey(a, bContraction.key)
        def contract(a1: Either[A, B]): Option[Type] = a1.fold(a1_ => Some(Left(a1_)), bContraction.contract(_).map(Right(_)))
        type Type = Either[A, bContraction.Type]
        val removed: Either[A, B] = k
        val fun: Fun[Either[Type, Unit], Either[A, B]] = Fun.rightMerge(bContraction.fun)

      }})
  def toStream = a.toStream.map(Left(_)) ++ b.toStream.map(Right(_))
}

object Key {
  implicit val noKey = NoKey
  implicit val unitKey = UnitKey
  implicit def eitherKey[A, B](implicit a: Key[A], b: Key[B]) = EitherKey(a, b)
}

trait KeyContraction[A] {
  val removed : A
  type Type <: A
  def contract(a1: A) : Option[Type]
  val fun: Fun[Either[Type, Unit], A]
  def key: Key[Type]
}

package id

object Key {
  implicit val noKey = NoKey
  implicit val unitKey = UnitKey
  implicit def eitherKey[A, B](implicit a: Key[A], b: Key[B]) = EitherKey(a, b)
}

sealed trait Key[K] {
  def total[V](f: K => V) : Total[K, V]
}
case object NoKey extends Key[Nothing] {
  def total[V](f: Nothing => V) = TotalNothing
}
case object UnitKey extends Key[Unit] {
  def total[V](f: Unit => V) = TotalUnit(f())
}
case class EitherKey[A, B](a: Key[A], b: Key[B]) extends Key[Either[A, B]] {
  def total[V](f: Either[A, B] => V) = TotalEither(a.total(f.compose(Left(_))), b.total(f.compose(Right(_)))) // TODO FIXME f get slower and slower as we recurse, so we are O(n log n) instead of O(n)
}


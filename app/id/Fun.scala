package id

/// Restricted functions that can be comapped on total maps efficiently
sealed trait Fun[-A, +B] extends Function[A, B] {
  def thenTotal[V](t: Total[B, V]) : Total[A, V]
  final def thenFun[C](f: Fun[B, C]) : Fun[A, C] = CompFun(f, this)
}
case class IdFun[A]() extends Fun[A, A] {
  def apply(a: A) = a
  def thenTotal[V](t: Total[A, V]) = t
}
case class CompFun[-A, +B, C](f: Fun[C, B], g: Fun[A, C]) extends Fun[A, B] {
  def apply(a: A) = f(g(a))
  def thenTotal[V](t: Total[B, V]) : Total[A, V] = g.thenTotal(f.thenTotal(t))
}
case class TwoFun[-A1, -A2, +B](f: Fun[A1, B], g: Fun[A2, B]) extends Fun[Either[A1, A2], B] {
  def apply(a: Either[A1, A2]) = a.fold(f, g)
  def thenTotal[V](t: Total[B, V]) : Total[Either[A1, A2], V] = TotalEither(f.thenTotal(t), g.thenTotal(t))
}
case class LeftFun[A]() extends Fun[A, Either[A, Nothing]] {
  def apply(a: A) = Left(a)
  def thenTotal[V](t: Total[Either[A, Nothing], V]) : Total[A, V] = t match {
    case TotalEither(a, _) => a
  }
}
case class RightFun[A]() extends Fun[A, Either[Nothing, A]] {
  def apply(a: A) = Right(a)
  def thenTotal[V](t: Total[Either[Nothing, A], V]) : Total[A, V] = t match {
    case TotalEither(_, a) => a
  }
}

object Fun {
  def eitherFun[A1, A2, B1, B2](f: Fun[A1, B1], g: Fun[A2, B2]): Fun[Either[A1, A2], Either[B1, B2]] =
    TwoFun(CompFun(LeftFun[B1](), f), CompFun(RightFun[B2](), g))
  def swapFun[A, B]: Fun[Either[A, B], Either[B, A]] =
    TwoFun(RightFun(), LeftFun())
}

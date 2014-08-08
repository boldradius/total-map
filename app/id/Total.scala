/*
   Copyright 2013 Tindr Solutions

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
*/
package id


// Restricted functions that can be comapped on total maps efficiently
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

/**
 * Total maps from K to V, where K is a type formed of Nothing, Unit, or Eithers
 * Unlike Map, Total is contravariant in K.
 */
sealed trait Total[-K, +V] extends Function[K, V] {
  def apply(k : K) : V
  def map[V2](f: V => V2): Total[K, V2]
  def update[K2 <: K, V2 >: V](k: K2, f: V2 => V2): Total[K2, V2]
  /**
   * Change to a new key type by restructuring the tree according to f. Can be used
   * to perform inserts into Total[K, V] by specifying default element using TotalEither(_, TotalUnit(v))
   * and then calling comap.
   */
  final def comap[K2, V2 >: V](f: Fun[K2, K]) : Total[K2, V2] = f.thenTotal(this)
  final def insert[K2, V2 >: V](f : Fun[K2, Either[K, Unit]], v: V2) : Total[K2, V2] = TotalEither(this, TotalUnit(v)).comap(f)
}
case object TotalNothing extends Total[Nothing, Nothing] {
  def apply(k : Nothing) = k
  def map[V2](f: Nothing => V2) = this
  def update[K2 <: Nothing, V2 >: Nothing](k: K2, f: V2 => V2) = k
}
case class TotalUnit[+V](v: V) extends Total[Unit, V] {
  def apply(k : Unit) = v
  def map[V2](f: V => V2) = TotalUnit(f(v))
  def update[K2 <: Unit, V2 >: V](k: K2, f: V2 => V2) = copy(v = f(v))
}
case class TotalEither[-K1, -K2, +V](t1: Total[K1, V], t2: Total[K2, V]) extends Total[Either[K1, K2], V] {
  def apply(k : Either[K1, K2]) = k.fold(t1, t2)
  def map[V2](f: V => V2) = TotalEither(t1.map(f), t2.map(f))
  def update[KE <: Either[K1, K2], V2 >: V](k: KE, f: V2 => V2): Total[KE, V2] =
    k.fold(k1 => copy(t1 = t1.update(k1, f)), k2 => copy(t2 = t2.update(k2, f)))
}

object Total {
  def apply[K : Key, V](f: K => V): Total[K, V] = implicitly[Key[K]].total(f)
  def contant[K : Key, V](v: V): Total[K, V] = apply[K, V]((_: K) => v)
}

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

/**
 * Total maps from K to V, where K must be in the Id typeclass.
 * Unlike Map, Total is contravariant in K.
 */
abstract class Total[-K, +V] {
  def insert[K3 >: K2, K2 <: K, V2 >: V](i: Insert[K2, K3], v: V2) = i.totalInsert(this, v)
  def map[V2](f: V => V2) : Total[K, V2]
  def update[K2 <: K, V2 >: V](k: K2, f: V2 => V2)(implicit i: Id[K2]) : Total[K2, V2] = Total.update(k, f)(i)(this)
}
case class TotalNothing[+V]() extends Total[Nothing, V] {
  def map[V2](f: V => V2) = TotalNothing()
}
case class TotalUnit[+V](v: V) extends Total[Unit, V] {
  def map[V2](f: V => V2) = TotalUnit(f(v))
}
case class TotalEither[-K1, -K2, +V](t1: Total[K1, V], t2: Total[K2, V]) extends Total[Either[K1, K2], V] {
  def map[V2](f: V => V2) = TotalEither(t1.map(f), t2.map(f))
}

object Total {
  def apply[K, V](id: Id[K], v: V) : Total[K, V] = apply((_:K) => v)(id)
  def apply[K, V](id: Id[K], f: K => V) : Total[K, V] = apply(f)(id)
  /** Create a total map from a (total) function */
  def apply[K : Id, V](f: K => V) : Total[K, V] = {
    def memoEither[B : Id, C : Id, V](f: Either[B, C] => V) : Total[Either[B, C], V] = 
      TotalEither(Total((kb : B) => f(Left(kb))), Total((kc : C) => f(Right(kc))))
    implicitly[Id[K]] match {
      case IdNothing() => TotalNothing[V]()
      case IdUnit() => TotalUnit(f({}))
      case IdEither(b, c) => memoEither(f)(b, c)
    }
  }
  // We're defining some external functions on Total to show that we can; that they do not have to be methods
  /** Update one element of the total map by applying a function to it. */
  def update[K : Id, V](k: K, f: V => V) : Total[K, V] => Total[K, V] = {
    def updateEither[B : Id, C : Id, V](k: Either[B, C], f: V => V) : Total[Either[B, C], V] => Total[Either[B, C], V] = 
	  {case TotalEither(tb, tc) => k.fold(kb => TotalEither(update(kb, f)(implicitly[Id[B]])(tb), tc), 
	     kc => TotalEither(tb, update(kc, f)(implicitly[Id[C]])(tc)))}
    implicitly[Id[K]] match {
      case IdNothing() => _ => TotalNothing[V]
      case IdUnit() => {case TotalUnit(v:V) => TotalUnit(f(v))}
      case IdEither(b, c) => updateEither(k, f)(b, c)
    }
  }
}

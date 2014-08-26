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
package com.boldradius.total


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
/* TODO Consider making this case applicable to any key, not just Unit.
  We'd rename it to TotalConstant.
  It would then allow a compact representation when the values are often the same.
  For example a Total[K, Boolean] could then efficiently represent a set where we
  are reminded to provide the missing Boolean values when K expands,
  unlike Set[K] where we are reminded to remove entries when K shrinks.
  We definitely do not want to add an Equals constraint on V everywhere however.
  Partial maps could then be efficiently represented as Total[K, Option[V]]*/
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

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

import id.Id._

/**
 * Witness for an Id type K2, that is just like K1 but has one more possible value. That value is newKey.
 * It also provides an efficient way to update a Total on K1 to a Total on K2.
 */
sealed trait Insert[K1, K2 >: K1] { // TODO rename to OneMore, so we can can rename InsertId to Insert
  val newKey: K2
  /**
   * Insert a key into a Total map, changing its type parameter in the process.
   */
  def totalInsert[V](t: Total[K1, V], v: V): Total[K2, V]
  /**
   * Removes a key from a Total map, changing its type parameter in the process.
   * Because Total is contravariant in K, instead of totalRemove(t) we can use 't' directly, 
   * but it will contain unreachable data. This may lead to problematic leaks if done systematically, 
   * but occasionally this could be helpful (more sharing).
   */
  def totalRemove[V](t: Total[K2, V]): Total[K1, V]
}
case class InsertLeft[B1, B2 >: B1, C](i: Insert[B1, B2]) extends Insert[Either[B1, C], Either[B2, C]] {
  val newKey = Left(i.newKey)
  def totalInsert[V](t: Total[Either[B1, C], V], v: V): Total[Either[B2, C], V] = t match {
    case TotalEither(t1, t2) => TotalEither(i.totalInsert(t1, v), t2)
  }
  def totalRemove[V](t: Total[Either[B2, C], V]): Total[Either[B1, C], V] = t match {
    case TotalEither(t1, t2) => TotalEither(i.totalRemove(t1), t2)
  }
}
case class InsertRight[B, C1, C2 >: C1](i: Insert[C1, C2]) extends Insert[Either[B, C1], Either[B, C2]] {
  val newKey = Right(i.newKey)
  def totalInsert[V](t: Total[Either[B, C1], V], v: V): Total[Either[B, C2], V] = t match {
    case TotalEither(t1, t2) => TotalEither(t1, i.totalInsert(t2, v))
  }
  def totalRemove[V](t: Total[Either[B, C2], V]): Total[Either[B, C1], V] = t match {
    case TotalEither(t1, t2) => TotalEither(t1, i.totalRemove(t2))
  }
}
case class InsertUnit() extends Insert[Nothing, Unit] {
  val newKey = {}
  def totalInsert[V](t: Total[Nothing, V], v: V) = TotalUnit(v)
  def totalRemove[V](t: Total[Unit, V]) = TotalNothing()
}
case class InsertEither[K2 >: Either[Nothing, Nothing]](i: Insert[Either[Nothing, Nothing], K2]) extends Insert[Nothing, K2] {
  val newKey = i.newKey
  def totalInsert[V](t: Total[Nothing, V], v: V): Total[K2, V] = i.totalInsert(TotalEither[Nothing, Nothing, V](TotalNothing(), TotalNothing()), v)
  def totalRemove[V](t: Total[K2, V]): Total[Nothing, V] = TotalNothing()
}

sealed trait InsertId_[K]
case class InsertId[K, K2 >: K](insert: Insert[K, K2], id: Id[K2]) extends InsertId_[K]
object InsertId {
  // Specialize the constructor for K = Nothing, because we have to be explicit about K=Nothing, but we may not know K2
  def insertNothing[K2](insert: Insert[Nothing, K2], id: Id[K2]) : InsertId_[Nothing] = InsertId[Nothing, K2](insert, id)
}

object Insert {
  def insertId[K](id: Id[K]) : InsertId_[K] =
    id match {
      case IdNothing() => 
        InsertId[Nothing, Either[Unit, Nothing]](InsertEither(InsertLeft[Nothing, Unit, Nothing](InsertUnit())), eitherId[Unit, Nothing](unitId, nothingId))
      case IdUnit() => 
        throw new RuntimeException("Id type is not extensible")
      case IdEither(b, c) => insertId(c) match {
        case InsertId(i, c2) => InsertId(InsertRight(i), eitherId(b, c2))
      }
    }
}

// TODO Change the insert function above so it creates of progressively deeper trees of eithers, not in a simple list of eithers:
// A nice was of mapping sets of integers to trees of eithers is the following:
// 0 | (1|2) | (3|4|5|6) | (7|8|9|10|11|12|13|14) ... where | represents Either and nests to the right
// Here each number is mapped to fixed place in the tree, no matter which other numbers are in the set
// a) it can grow indefinitely keeping previous types as subtypes, in fact all subsets are subtypes
// b) it is easily ordered (the default Ord on Either does the job)

object Remove {
  /** The removal witness is the same a the insertion witness, where the input and output types are exchanged. */
  type Remove[K1, K2 <: K1] = Insert[K2, K1]
  // TODO: generate a Remove[K1, K2] from any Id[K1] and K1
}

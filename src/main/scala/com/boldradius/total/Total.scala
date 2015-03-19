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

package ops {

trait Extension[K <: AnyId, +V] {
  val total: Total[V] {type Id >: K <: AnyId}
  def newIds: Seq[total.Id]

  def mapKeys(f: Seq[total.Id] => Seq[total.Id]): Extension[K, V] =
    Extension.lazyKeys[K, V](total)(f(newIds))
}

trait SingleExtension[K <: AnyId, +V] extends Extension[K, V] {
  final def newIds = List(newId)
  val newId: total.Id
}

object Extension {
  def apply[K <: AnyId, V](total_ : Total[V] {type Id >: K <: AnyId})(keys_ : Seq[total_.Id]) = new Extension[K, V] {
    val total: total_.type = total_
    val newIds: Seq[total.Id] = keys_
  }
  def lazyKeys[K <: AnyId, V](total_ : Total[V] {type Id >: K <: AnyId})(keys_ : => Seq[total_.Id]) = new Extension[K, V] {
    val total: total_.type = total_
    lazy val newIds: Seq[total.Id] = keys_
  }
}

object SingleExtension {
  def apply[K <: AnyId, V](total_ : Total[V] {type Id >: K <: AnyId})(key_ : total_.Id) = new SingleExtension[K, V] {
    val total: total_.type = total_
    val newId: total.Id = key_
  }
}

trait Contraction[K <: AnyId, +V] {
  val total: Total[V] {type Id <: K}

  def removed: Seq[(K, V)]

  def filter(k: K): Option[total.Id]
}

trait SingleContraction[K <: AnyId, +V] extends Contraction[K, V] {
  val total: Total[V] {type Id <: K}
  val removedKey: K
  val removedValue: V

  def removed = List((removedKey, removedValue))

  override def filter(k: K) =
    if (k.v == removedKey.v) None
    else Some(k.asInstanceOf[total.Id]) // This cast performed for performance reasons. O(nesting-level) proof seams too slow.
}

}

import ops._


/**
 * An immutable indexable collection of values of type `V`. To index in the collection,
 * the apply method must be called with an identifier of the member type ``Id``.
 * Insertion in the collection produces a new collection with a different `Id` type that
 * is a supertype of the original `Id` type. Conversely removal from the collection
 * produces a new collection with an `Id` type that is a subtype or the original.
 * @tparam V The type of the elements in the collection
 */
sealed trait Total[+V] {
  /**
   * The type for identifiers that may be used to index in the collection.
   */
  type Id <: AnyId

  /**
   * The type for identifiers that may be inserted in the collection
   */
  type Comp <: AnyId
  /**
   * The number of elements in the collection. This size is accessible in constant time.
   */
  val size : Int

  /**
   * Determines if the collection is empty.
   */
  def isEmpty : Boolean = size == 0

  /**
   * Indexes in the collection.
   * @param id Any identifier in the `Id` type of this collection.
   * @return The value in the collection that corresponds to the id.
   *         The existence of this value within the collection is guaranteed,
   *         so no exceptions will ever be thrown due to an invalid id
   *         (unless null is passed in).
   */
  def apply(id : Id) : V

  /**
   * Narrows the type of a specific id down to the id in the collection ``Id`` type.
   * Since it is a narrowing conversion, it may fail, in which case None is returned.
   * @param id Any id
   * @return The same id, converted to the collection's `Id` type, or None if the
   *         id is not part of the collection.
   */
  def narrowId(id: AnyId) : Option[Id]

  /**
   * Applies a function to every element of the collection and produces a
   * new collection containing the resulting values, in corresponding
   * positions (under the same id). The Id type is unchanged.
   * @param f The function to be applied to all the elements
   * @tparam V2 The type of the resulting elements
   * @return The new ``Total``, with the same `Id` type.
   */
  def map[V2](f: V => V2): Total[V2] {type Id = Total.this.Id}
  /**
   * Updates a particular element of the collection. Applies a function to this
   * element and produces a new collection where the element is replaced by the
   * result of the function application. The Id type is unchanged.
   * @param id The id of the element to be replaced
   * @param f The function to be applied to element at ``id``
   * @tparam V2 The type of the resulting elements
   * @return The new ``Total``, with the same `Id` type.
   */
  def update[V2 >: V](id: Id, f: V2 => V2): Total[V2] {type Id = Total.this.Id}

  /**
   * Allocates an unused id. Since it is not currently in the collection, it
   * is of type `Comp`.
   * @return
   */
  def allocate : Comp

  /**
   * Adds an element to the collection at the specified id, which must not
   * already be in the collection. The resulting `Total` has
   * an `Id` type which is a supertype of the original `Id` type, so any
   * values of the old `Id` type that were kept in other data structures,
   * may simply be upcasted to the new `Id` type.
   * @param id The new id where the value will be added to the collection.
   *           It must not currently be in the collection, so its type must must be `Comp`.
   *           Use `allocate` to obtain such an id.
   * @param value The value to be inserted in the collection
   * @tparam V2 The type of the value to insert, which will be the value type of the
   *            resulting collection.
   * @return The resulting Total
   */
  def insertAt[V2 >: V](id: Comp, value: V2): TotalSuper[Id, V2]

  /**
   * Adds an element to the collection. An id is automatically allocated,
   * according to some unspecified order. The resulting `Total` has
   * an `Id` type which is a supertype of the original `Id` type, so any
   * values of the old `Id` type that were kept in other data structures,
   * may simply be upcasted to the new `Id` type.
   * @param value The value to be inserted in the collection
   * @tparam V2 The type of the value to insert, which will be the value type of the
   *            resulting collection.
   * @return A `SingleExtension` which captures the two (dependent) results:
   *         the resulting Total, and the newly allocated id
   */
  def insert[V2 >: V](value: V2): SingleExtension[Id, V2]

  /**
   * Partitions a `Total` in two according to a classification function.
   * The function classifies each value of the collection for transfer to
   * one of the two results. The ids of classified values remain the same.
   * The `Id` types of the two resulting `Total` are both subtypes of
   * original `Id` type.
   * @param f The function that classified each element as a `Left` or a `Right`
   * @tparam A The value type of the ``Total`` for the `Left` class.
   * @tparam B The value type of the ``Total`` for the `Left` class.
   * @return A tuple of the two resulting ``Total`` (left and right).
   */
  def partition[A, B](f: V => Either[A, B]) : (TotalSub[Id, A], TotalSub[Id, B])

  /**
   * Filters out all elements whose id is also present in `t`.
   * @param t A `Total` containing the identifiers to be removed.
   * @return A `Total` which only contains the elements with identifiers not in `t`
   */
  def difference(t: Total[_]) : TotalSub[Id, V]

  /**
   * A stream of all the elements in the collection, as tuples of id and value.
   * @return A stream of all the elements in the collection, as tuples of id and value.
   */
  def toStream : Stream[(Id, V)]
  private[total] def removeInner(removedKey: Id) : (TotalSub[Id, V], V)

  /**
   * Adds a sequence of elements to the collection. A corresponding number of
   * ids are automatically allocated, according to some unspecified order.
   * The resulting `Total` has an `Id` type which is a supertype of the
   * original `Id` type, so any values of the old `Id` type that were kept
   * in other data structures, may simply be upcasted to the new `Id` type.
   * @param values The values to be inserted in the collection
   * @tparam V2 The type of the values to insert, which will be the value type of the
   *            resulting collection.
   * @return An `Extension` which captures the two (dependent) results:
   *         the resulting Total, and the newly allocated ids (in the same order)
   */
  def insertAll[V2 >: V](values: Seq[V2]): Extension[Id, V2] = {
    values.foldLeft[Extension[Id, V2]](
      Extension[Id, V2](this)(Nil)) {
      (extension, value) =>
        val singleExtension = extension.total.insert(value)
        Extension[Id, V2](singleExtension.total)(singleExtension.newId +: extension.newIds): Extension[Id, V2]
    }.mapKeys(_.reverse)
  }
  /**
   * Removes an element from the collection, at a specific id. The resulting `Total` has
   * an `Id` type which is a subtype of the original `Id` type. Any
   * values of the old `Id` type that were kept in other data structures,
   * may be narrowed down with the returned filtering function, which may be more efficient
   * that ``narrowId``.
   * @param removedKey The identifier for element to be removed.
   * @return A `SingleContraction` which captures the (dependent) results:
   *         the resulting Total, the removed value, the removed key, and the filtering function
   */
  def remove(removedKey: Id): SingleContraction[Id, V] = {
    val removedInner = removeInner(removedKey)
    val k = removedKey
    new SingleContraction[Id, V] {
      type ContractionKey = Id
      val total = removedInner._1
      val removedValue: V = removedInner._2
      val removedKey = k
    }
  }

  /**
   * Filters the collections according to a predicate. All and only the elements for which the
   * predicate returns true will be in the resulting total. The `Id` type of the result
   * is a subtype of the original `Id` type. NOTE It is often preferable to use `keep`
   * instead of `filter`, because it allows the resulting collection to have a distinct value type,
   * which is typically narrower, and reflects what was learned as a result of running the test function.
   * @param p The predicate function, which is true for elements to be included
   * @return The new collection with a subset of the original elements.
   */
  def filter(p: V => Boolean) = partition(v => if (p(v)) Right(v) else Left(()))._2  // TODO: Define an optimized version, partition rebuilds the whole tree
  /**
   * Keeps all elements that can be converted to a new type with a provided
   * test function. The function returns an `Option` of the new type, which is
   * `None` when elements fail the test and must be discarded. The `Id` type of the result
   * is a subtype of the original `Id` type.
   * @param test The test function, which returns ``Some`` for elements to be included, and ``None`` otherwise
   * @return The new collection with a subset of the original elements.
   */
  def keep[V2](test: V => Option[V2]) = partition(test(_).toRight(()))
}
private[total] case object TotalNothing extends Total[Nothing] {
  type Id = Nothing
  type Comp = AnyId
  val size = 0
  def apply(k : Nothing) = k
  def narrowId(id: AnyId) = None
  def map[V2](f: Nothing => V2) = this
  def update[V2 >: Nothing](k: Id, f: V2 => V2) = k
  def allocate : Comp = Id2.zero
  def insertAt[V2 >: Nothing](id: Comp, value: V2): TotalSuper[Id, V2] =
    id.ofId2.fold[TotalSuper[Id, V2]](_ => TotalWith[V2, Nothing, Nothing](value, TotalNothing, TotalNothing),
      i1 => {val i=TotalNothing.insertAt[V2](i1, value); TotalWithout[V2, i.Id, Nothing](i, TotalNothing)},
      i2 => {val i=TotalNothing.insertAt[V2](i2, value); TotalWithout[V2, Nothing, i.Id](TotalNothing, i)})
  def insert[V2 >: Nothing](value: V2) : SingleExtension[Id, V2] = {
    val inter = TotalWith[V2, Nothing, Nothing](value, TotalNothing, TotalNothing)
    SingleExtension[Id, V2](inter)(Id2.zero)
  }
  def removeInner(key: Nothing) = key
  def difference(t: Total[_]) = TotalNothing
  def partition[A, B](f: Nothing => Either[A, B]) : (TotalSub[Id, A], TotalSub[Id, B]) = (Total.empty, Total.empty)
  def toStream = Stream.empty
}
private[total] case class TotalWith[+V, K1 <: AnyId, K2 <: AnyId](v: V, t1: Total[V] {type Id = K1}, t2: Total[V] {type Id = K2}) extends Total[V] {
  type Id = Id2[Unit, K1, K2]
  type Comp = Id2[Nothing, t1.Comp, t2.Comp]
  val size = 1 + t1.size + t2.size
  def apply(k : Id) = k.fold(_ => v, t1(_), t2(_))
  def narrowId(id: AnyId) =
    id.ofId2.fold[Option[Id]](
      _ => Some(Id2.zero),
      i1 => t1.narrowId(i1).map(Id2.in1(_)),
      i2 => t2.narrowId(i2).map(Id2.in2(_)))
  def map[V2](f: V => V2) = TotalWith(f(v), t1.map(f), t2.map(f))
  def update[V2 >: V](k: Id, f: V2 => V2) = {
    k.fold[Total[V2] {type Id = TotalWith.this.Id}](
      _ => TotalWith[V2, K1, K2](f(v), t1, t2),
      k1 => {
        val t1a = t1.update(k1, f)
        TotalWith[V2, K1, K2](v, t1a, t2)},
      k2 => {
        val t2a = t2.update(k2, f)
        TotalWith[V2, K1, K2](v, t1, t2a)})
  }
  def allocate : Comp =
    if (t1.size <= t2.size) Id2.in1(t1.allocate)
    else Id2.in2(t2.allocate)
  def insertAt[V2 >: V](id: Comp, value: V2): TotalSuper[Id, V2] =
    id.fold[TotalSuper[Id, V2]](identity _,
      i1 => {val i=t1.insertAt[V2](i1, value); TotalWith[V2, i.Id, t2.Id](v, i, t2)},
      i2 => {val i=t2.insertAt[V2](i2, value); TotalWith[V2, t1.Id, i.Id](v, t1, i)})
  def insert[V2 >: V](value: V2) =
    if (t1.size <= t2.size) {
      val ext = t1.insert(value)
      SingleExtension[Id, V2](TotalWith[V2, ext.total.Id, K2](v, ext.total, t2))(Id2.in1(ext.newId))
    }
    else {
      val ext = t2.insert(value)
      SingleExtension[Id, V2](TotalWith[V2, K1, ext.total.Id](v, t1, ext.total))(Id2.in2(ext.newId))
    }
  def removeInner(removedKey: Id): (TotalSub[Id, V], V) =
    removedKey.fold[(TotalSub[Id, V], V)](_ =>
      (Total.pair[V, t1.Id, t2.Id](t1, t2), v),
      k1 => {
        val (t1a, removed) = t1.removeInner(k1)
        (TotalWith[V, t1a.Id, t2.Id](v, t1a, t2), removed)
      }, k2 => {
        val (t2a, removed) = t2.removeInner(k2)
        (TotalWith[V, t1.Id, t2a.Id](v, t1, t2a), removed)
      })
  def difference(d: Total[_]) : TotalSub[Id, V] =
    if (d.isEmpty) this
    else d match {
      case TotalWith(_, d1, d2) => Total.pair(t1.difference(d1), t2.difference(d2))
      case TotalWithout(d1, d2) => TotalWith(v, t1.difference(d1), t2.difference(d2))
      case _ => this
    }
  def partition[A, B](f: V => Either[A, B]) : (TotalSub[Id, A], TotalSub[Id, B]) = {
    val s1 = t1.partition(f)
    val s2 = t2.partition(f)
    f(v).fold(
      a => (TotalWith(a, s1._1, s2._1), TotalWithout(s1._2, s2._2)),
      b => (TotalWithout(s1._1, s2._1), TotalWith(b, s1._2, s2._2)))
  }
  def toStream : Stream[(Id, V)] = // TODO: revisit method name, the order is surprising. Improve algorithm
    (Id2.zero, v) #:: (t1.toStream.map{case(k, v) => (Id2.in1(k), v)} : Stream[(Id, V)]).append(t2.toStream.map{case(k, v) => (Id2.in2(k), v)})
}
private[total] case class TotalWithout[+V, K1 <: AnyId, K2 <: AnyId](t1: Total[V] {type Id = K1}, t2: Total[V] {type Id = K2}) extends Total[V] {
  type Id = Id2[Nothing, K1, K2]
  type Comp = Id2[Unit, t1.Comp, t2.Comp]
  val size = t1.size + t2.size
  def apply(k : Id) = k.fold((n: Nothing) => n, t1(_), t2(_))
  def narrowId(id: AnyId) =
    id.ofId2.fold[Option[Id]](
      _ => None,
      i1 => t1.narrowId(i1).map(Id2.in1(_)),
      i2 => t2.narrowId(i2).map(Id2.in2(_)))
  def map[V2](f: V => V2) = {
    val t1a = t1.map(f)
    val t2a = t2.map(f)
    TotalWithout[V2, t1a.Id, t2a.Id](t1a, t2a)
  }
  def update[V2 >: V](k: Id, f: V2 => V2) = {
    k.fold[Total[V2] {type Id = TotalWithout.this.Id}](
      (n: Nothing) => n,
      k1 => {
        val t1a = t1.update(k1, f)
        TotalWithout[V2, t1a.Id, t2.Id](t1a, t2)},
      k2 => {
        val t2a = t2.update(k2, f)
        TotalWithout[V2, t1.Id, t2a.Id](t1, t2a)})
  }
  def allocate : Comp = Id2.zero
  def insertAt[V2 >: V](id: Comp, value: V2): TotalSuper[Id, V2] =
    id.fold[TotalSuper[Id, V2]](_ => TotalWith[V2, t1.Id, t2.Id](value, t1, t2),
      i1 => {val i=t1.insertAt[V2](i1, value); TotalWithout[V2, i.Id, t2.Id](i, t2)},
      i2 => {val i=t2.insertAt[V2](i2, value); TotalWithout[V2, t1.Id, i.Id](t1, i)})
  def insert[V2 >: V](value: V2) =
    new SingleExtension[Id, V2] {
      val total = TotalWith[V2, t1.Id, t2.Id](value, t1, t2)
      val newId : total.Id = Id2.zero
    }
  def removeInner(removedKey: Id): (TotalSub[Id, V], V) =
    removedKey.fold[(TotalSub[Id, V], V)]((n: Nothing) => n,
      k1 => {
        val (t1a, v) = t1.removeInner(k1)
        (TotalWithout[V, t1a.Id, t2.Id](t1a, t2), v)
      }, k2 => {
        val (t2a, v) = t2.removeInner(k2)
        (TotalWithout[V, t1.Id, t2a.Id](t1, t2a), v)
      })
  def difference(d: Total[_]) : TotalSub[Id, V] =
    if (d.isEmpty) this
    else d match {
      case TotalWith(_, d1, d2) => Total.pair(t1.difference(d1), t2.difference(d2))
      case TotalWithout(d1, d2) => Total.pair(t1.difference(d1), t2.difference(d2))
      case _ => this
    }
  def partition[A, B](f: V => Either[A, B]) : (TotalSub[Id, A], TotalSub[Id, B]) = {
    val s1 = t1.partition(f)
    val s2 = t2.partition(f)
    (TotalWithout(s1._1, s2._1), TotalWithout(s1._2, s2._2))
  }
  def toStream : Stream[(Id, V)] = // TODO: revisit method name, the order is surprising. Improve algorithm
    (t1.toStream.map{case(k, v) => (Id2.in1(k), v)} : Stream[(Id, V)]).append(t2.toStream.map{case(k, v) => (Id2.in2(k), v)})
}

object Total {
  val empty = TotalNothing
  private[total] def pair[V, K1 <: AnyId, K2 <: AnyId](t1: Total[V] {type Id = K1}, t2: Total[V] {type Id = K2}) : Total[V] {type Id <: Id2[Nothing, K1, K2]} =
    if (t1.isEmpty && t2.isEmpty) TotalNothing else TotalWithout[V, t1.Id, t2.Id](t1, t2)
}


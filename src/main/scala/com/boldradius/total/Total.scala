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

sealed trait Total[+V] {
  type Id <: Alt3[_, _, _]
  val size : Int
  def apply(k : Id) : V
  def map[V2](f: V => V2): Total[V2] {type Id = Total.this.Id}
  def update[V2 >: V](k: Id, f: V2 => V2): Total[V2] {type Id = Total.this.Id}
  def insert[V2 >: V](value: V2): SingleExtension[V2] {val original : Total.this.type; val total: Total[V2] {type Id >: original.Id <: Alt3[_, _, _]}  }
  def insertAll[V2 >: V](values: Seq[V2]): Extension[V2] {val original : Total.this.type; val total: Total[V2] {type Id >: original.Id <: Alt3[_, _, _]}} = {
    type R = Extension[V2] {val original: Total.this.type; val total: Total[V2] {type Id >: original.Id <: Alt3[_, _, _]}}
    values.foldLeft[R](
      Extension[V2, this.type](this)(this)(Nil)) {
      (extension, value) =>
        val singleExtension = extension.total.insert(value)
        Extension[V2, this.type](this)(singleExtension.total)(singleExtension.newId +: extension.newIds): R
    }.mapKeys(_.reverse)
  }
  def remove(removedKey: Id): Contraction[Id, V] = {
    val removed = removeInner(removedKey)
    val k = removedKey
    new Contraction[Id, V] {
      type ContractionKey = Id
      val total = removed._1.asInstanceOf[Total[V] {type Id <: ContractionKey}] // TODO: prove this properly
      val removedValue: V = removed._2
      val removedKey = k
    }
  }
  private[total] def removeInner(removedKey: Id) : (Total[V], V) // TODO put back {type Id <: Total.this.Id} refinement
  def toStream : Stream[(Id, V)]
  // TODO: filter, filterOne

}
case object TotalNothing extends Total[Nothing] {
  type Id = Nothing
  val size = 0
  def apply(k : Nothing) = k
  def map[V2](f: Nothing => V2) = this
  def update[V2 >: Nothing](k: Id, f: V2 => V2) = k
  def insert[V2 >: Nothing](value: V2) : SingleExtension[V2] {val original : TotalNothing.type} = {
    val inter = TotalWithout[V2, Nothing, Nothing](TotalNothing, TotalNothing)
    val i = inter.insert(value)
    SingleExtension[V2, this.type](this)(i.total)(i.newId)
  }
  def removeInner(key: Nothing) = key
  def toStream = Stream.empty
}
case class TotalWith[+V, K1 <: Alt3[_, _, _], K2 <: Alt3[_, _, _]](v: V, t1: Total[V] {type Id = K1}, t2: Total[V] {type Id = K2}) extends Total[V] {
  type Id = Alt3[Unit, K1, K2]
  val size = 1 + t1.size + t2.size
  def apply(k : Id) = k.fold(_ => v, t1(_), t2(_))
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
  def insert[V2 >: V](value: V2) =
    if (t1.size <= t2.size) {
      val ext = t1.insert(value)
      SingleExtension[V2, this.type](this)(TotalWith[V2, ext.total.Id, K2](v, ext.total, t2))(Alt3.in1(ext.newId))
    }
    else {
      val ext = t2.insert(value)
      SingleExtension[V2, this.type](this)(TotalWith[V2, K1, ext.total.Id](v, t1, ext.total))(Alt3.in2(ext.newId))
    }
  def removeInner(removedKey: Id): (Total[V], V) =
    removedKey.fold[(Total[V], V)](_ =>
      (TotalWithout[V, t1.Id, t2.Id](t1, t2), v),
      k1 => {
        val (t1a, removed) = t1.removeInner(k1)
        (TotalWith[V, t1a.Id, t2.Id](v, t1a, t2), removed)
      }, k2 => {
        val (t2a, removed) = t2.removeInner(k2)
        (TotalWith[V, t1.Id, t2a.Id](v, t1, t2a), removed)
      })
  def toStream : Stream[(Id, V)] = // TODO: revisit method name, the order is surprising. Improve algorithm
    (Alt3.zero, v) #:: (t1.toStream.map{case(k, v) => (Alt3.in1(k), v)} : Stream[(Id, V)]).append(t2.toStream.map{case(k, v) => (Alt3.in2(k), v)})
}
case class TotalWithout[+V, K1 <: Alt3[_, _, _], K2 <: Alt3[_, _, _]](t1: Total[V] {type Id = K1}, t2: Total[V] {type Id = K2}) extends Total[V] {
  type Id = Alt3[Nothing, K1, K2]
  val size = t1.size + t2.size
  def apply(k : Id) = k.fold((n: Nothing) => n, t1(_), t2(_))
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
  def insert[V2 >: V](value: V2) =
    new SingleExtension[V2] {
      val original : TotalWithout.this.type = TotalWithout.this
      val total : Total[V2] {type Id = Alt3[Unit, t1.Id, t2.Id]} = TotalWith[V2, t1.Id, t2.Id](value, t1, t2)
      val newId : total.Id = Alt3.zero
    }
  def removeInner(removedKey: Id): (Total[V], V) =
    removedKey.fold[(Total[V], V)]((n: Nothing) => n,
      k1 => {
        val (t1a, v) = t1.removeInner(k1)
        (TotalWithout[V, t1a.Id, t2.Id](t1a, t2), v)
      }, k2 => {
        val (t2a, v) = t2.removeInner(k2)
        (TotalWithout[V, t1.Id, t2a.Id](t1, t2a), v)
      })
  def toStream : Stream[(Id, V)] = // TODO: revisit method name, the order is surprising. Improve algorithm
    (t1.toStream.map{case(k, v) => (Alt3.in1(k), v)} : Stream[(Id, V)]).append(t2.toStream.map{case(k, v) => (Alt3.in2(k), v)})
}

object Total {
  val empty = TotalNothing
}

trait Extension[+V] {
  val original : Total[V]
  val total: Total[V] {type Id >: original.Id <: Alt3[_, _, _]}
  val newIds : Seq[total.Id]
  def mapKeys(f: Seq[total.Id] => Seq[total.Id]) : Extension[V] {val original : Extension.this.original.type} =
    Extension[V, original.type](original)(total)(f(newIds))
}
trait SingleExtension[+V] extends Extension[V] {
  final val newIds = List(newId)
  val newId : total.Id
}
object Extension {
  def apply[V, T <: Total[V]](original_ : T)(total_ : Total[V] {type Id >: original_.Id <: Alt3[_, _, _]})(keys_ : Seq[total_.Id]) = new Extension[V] {
    val original : original_.type = original_
    val total : total_.type = total_
    val newIds : Seq[total.Id] = keys_
  }
}
object SingleExtension {
  def apply[V, T <: Total[V]](original_ : T)(total_ : Total[V] {type Id >: original_.Id <: Alt3[_, _, _]})(key_ : total_.Id) = new SingleExtension[V] {
    val original : original_.type = original_
    val total : total_.type = total_
    val newId : total.Id = key_
  }
}

trait Contraction[K <: Alt3[_, _, _], +V] {
  val total: Total[V] {type Id <: K}
  val removedKey : K
  val removedValue : V
  def filter(k: K) : Option[total.Id] =
    if (k.v == removedKey.v) None
    else Some(k.asInstanceOf[total.Id]) // This cast performed for performance reasons. O(nesting-level) proof seams too slow.
}
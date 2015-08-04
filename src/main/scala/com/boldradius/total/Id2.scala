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
 * We would like to create a type Comp[-T < : Id2] which produces the complement
 * of type T, a type inhabited by all Id2 instances not in T. This does not appear possible,
 * even if the universe was made up of two types (instead of Id2).
 * Defining it in terms of a type member (T#C) seems necessary, but we want the T's to be
 * related by subtyping, and we cannot define C concretely both in base classes and subclasses.
 *
 * There are additional complications with infinite universes such as Id2.
 */

class Id2[+U <: Unit, +A1, +A2] private (val v: Long) extends AnyVal {
  def in1 = Id2.in1(this)
  def in2 = Id2.in2(this)
  def fold[Z](e: U => Z, a1: A1 => Z, a2: A2 => Z) : Z =
    if (v == 0) e(().asInstanceOf[U])
    else if ((v & 1) != 0) a1(new Id2(v >> 1).asInstanceOf[A1])
    else a2(new Id2((v >> 1) - 1).asInstanceOf[A2])
  def ofId2 = this.asInstanceOf[Id2[_ <: Unit, _ <: A1 with Id2[_, _, _], _ <: A2 with Id2[_, _, _]]]
  override def toString : String = "Id(" + v + ")"
}

object Id2 {
  def in1[A <: Id2[_, _, _]](id: A) : Id2[Nothing, A, Nothing] = new Id2(id.v * 2 + 1)
  def in2[A <: Id2[_, _, _]](id: A) : Id2[Nothing, Nothing, A] = new Id2(id.v * 2 + 2)
  val zero : Id2[Unit, Nothing, Nothing] = new Id2(0)
  // Numbering occurs in this order. Same you obtain by picking branch of minimum size, biased to the left.
  //     0
  //  1   2
  // 3 5 4 6
}


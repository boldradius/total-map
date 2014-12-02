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


class Alt3[+U <: Unit, +A1 <: Alt3[_, _, _] , +A2 <: Alt3[_, _, _]] private (val v: Long) extends AnyVal {
  def in1 = Alt3.in1(this)
  def in2 = Alt3.in2(this)
  def fold[Z](e: U => Z, a1: A1 => Z, a2: A2 => Z) : Z =
    if (v == 0) e(().asInstanceOf[U])
    else if ((v & 1) != 0) a1(new Alt3(v >> 1).asInstanceOf[A1])
    else a2(new Alt3((v >> 1) - 1).asInstanceOf[A2])
  override def toString : String = "Id(" + v + ")"
}

object Alt3 {
  def in1[A <: Alt3[_, _,_]](id: A) : Alt3[Nothing, A, Nothing] = new Alt3(id.v * 2 + 1)
  def in2[A <: Alt3[_, _,_]](id: A) : Alt3[Nothing, Nothing, A] = new Alt3(id.v * 2 + 2)
  val zero : Alt3[Unit, Nothing, Nothing] = new Alt3(0)
  // Numbering occurs in this order. Same you obtain by picking branch of minimum size, biased to the left.
  //     0
  //  1   2
  // 3 5 4 6
}


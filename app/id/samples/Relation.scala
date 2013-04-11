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
package id.samples

import id._
import id.Id._
import id.Insert._

sealed trait Relation_ {
  /** Adds a new element in the relation, in relation with itself only.*/
  def addElement : Relation_ = this match {
    case Relation(m, a) => insertId(a) match {
      case InsertId(ab, b) => Relation(
          m.map(_.insert(ab, false))
            .insert(ab, Total(a, false).insert(ab, true)), 
          b)
    }
  }
}
case class Relation[A](totalMap: Total[A, Total[A, Boolean]], id: Id[A]) extends Relation_

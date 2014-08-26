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

import com.boldradius.total._
import scala.language.existentials

case class Relation[A](total: Total[A, Total[A, Boolean]], id: Id[A]) {
  /** Adds a new element in the relation, in relation with itself only.*/
  def addElement = {
    val i = id.insert
    Relation(
        total.map(_.insert(i.fun, false))
          .insert(i.fun, Total.contant(false)(id.key).insert(i.fun, true)),
        i.id)
  }
}

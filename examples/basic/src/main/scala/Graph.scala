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

case class Graph[A](id: Total[Unit] {type Id = A}, edges: List[(A, A)]) {
  /** Adds a new node with an edge to itself. */
  def addNode = {
    val i = id.insert(())
    Graph(i.total, (i.newId, i.newId) :: edges)
  }
}

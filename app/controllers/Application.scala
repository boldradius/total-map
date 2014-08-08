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
package controllers

import play.api._
import play.api.mvc._
import id._
import id.samples._

object Application extends Controller {
  var a : Graph_ = Graph[Nothing](NothingId, Nil)
  def index = Action {
    (1 to 100000).foreach(_ => a = a.addNode) // TODO: Investigate performance/memory-usage, has trouble around 1M to 2M,
    
    Ok(views.html.index(
        a match {case Graph(id, edges) => edges.size.toString}))
  }

}

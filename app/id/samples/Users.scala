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

sealed trait Users_ {
  /** Adds a new user with no friends.*/
  def newUser(name: String) : Users_ = this match {
    case Users(total, a) => insertId(a) match {
      case InsertId(ab, b) => Users(total.insert(ab, User(name, Nil)), b)
    }
  }
}
case class Users[A](total: Total[A, User[A]], id: Id[A]) extends Users_
case class User[+A](name: String, friends: List[A])
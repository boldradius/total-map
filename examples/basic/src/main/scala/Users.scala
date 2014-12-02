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

case class User[+A](name: String, friends: List[A])

case class Users[A](total: Total[User[A]] {type Id = A}, id: Total[Unit]) {
  /*
  /** Adds a new user with no friends.*/
  def addUser(user: User[A]) = {
    val extension = id.insert
    Users(total.insert(extension.fun, user), extension.id)
  }
  */
}

object Users {
  def renderUserProfile[A](user: User[A], users: Users[A]): String =
    Seq(
      user.name,
      ("Friends: " :: user.friends.map(users.total(_).name))).mkString("\n")
}



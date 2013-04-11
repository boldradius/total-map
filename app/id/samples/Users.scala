package id.samples

import id._
import id.Id._
import id.Insert._

sealed trait Users_ {
  /** Adds a new element in the relation, in relation with itself only.*/
  def newUser(name: String) : Users_ = this match {
    case Users(total, a) => insertId(a) match {
      case InsertId(ab, b) => Users(total.insert(ab, User(name, Nil)), b)
    }
  }
}
case class Users[A](total: Total[A, User[A]], id: Id[A]) extends Users_
case class User[+A](name: String, friends: List[A])
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

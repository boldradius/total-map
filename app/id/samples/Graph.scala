package id.samples

import id._
import id.Id._
import id.Insert._

sealed trait Graph_ {
  /** Adds a new node with an edge to itself. */ 
  def addNode = this match {
    case Graph(a, edges) => insertId(a) match {
      case InsertId(ab, b) => Graph(b, (ab.newKey, ab.newKey) :: edges)
    }
  }
}
case class Graph[A](id: Id[A], edges: List[(A, A)]) extends Graph_

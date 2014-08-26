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


sealed trait Id[A] {
  val minDepth : Int
  def key : Key[A]
  def insert = extend[Unit]
  def extend[N : Key]: Extension[A, N]
}
trait Extension[A, N] {
  type Type >: A
  def newValues(n : N) : Type
  val fun: Fun[Type, Either[A, N]]
  val id: Id[Type]
}
object Extension {
  trait ExtensionUnitValue[T] {val newValue : T}
  implicit def extensionUnit[A](v: Extension[A, Unit]) = new ExtensionUnitValue[v.Type]{
    val newValue : v.Type = v.newValues(())
  }
}

case object NothingId extends Id[Nothing] {
  val minDepth = 0
  def key = implicitly
  def extend[N : Key]: Extension[Nothing, N] = new Extension[Nothing, N] {
    type Type = Either[N, Either[Nothing, Nothing]]
    def newValues(n: N) = Left(n)
    lazy val fun : Fun[Type, Either[Nothing, N]] = TwoFun(RightFun(), TwoFun[Nothing, Nothing, Either[Nothing, N]](LeftFun() , LeftFun()))
    val id : Id[Type] = IdNode[N, Nothing, Nothing](implicitly[Key[N]], NothingId, NothingId)
  }
}
case class IdNode[A, B, C](a : Key[A], b: Id[B], c: Id[C]) extends Id[Either[A, Either[B, C]]] {
  val minDepth = (a : Key[_]) match {
    case NoKey => 0
    case _ => Math.min(b.minDepth, c.minDepth) + 1
  }
  def key = EitherKey(a, EitherKey(b.key, c.key))
  def extend[N : Key]: Extension[Either[A, Either[B, C]], N] = IdNode.combine(this)
}

object IdNode {
  def combine[A, B, C, N : Key](n: IdNode[A, B, C]) : Extension[Either[A, Either[B, C]], N] = {
    n.a match {
      case NoKey => combineNothing[B, C, N](n)
      case _ => {
        def up[X](g : Fun[X, Either[Either[B, C], N]]) : Fun[Either[A, X], Either[Either[A, Either[B, C]], N]] =
          TwoFun(
            LeftFun[A]() thenFun LeftFun(),
            g thenFun TwoFun(RightFun() thenFun LeftFun(), RightFun()))
        if (n.b.minDepth <= n.c.minDepth) {
          val s = n.b.extend
          new Extension[Either[A, Either[B, C]], N] {
            type Type = Either[A, Either[s.Type, C]]
            def newValues(n : N) = Right(Left(s.newValues(n)))
            lazy val fun = up(f2) // TODO evaluate performance
            def f2 : Fun[Either[s.Type, C], Either[Either[B, C], N]] = TwoFun(
              s.fun thenFun TwoFun(
                LeftFun[B]() thenFun LeftFun(),
                RightFun[N]()),
              RightFun[C]() thenFun LeftFun())
            val id = n.copy(b = s.id)
          }
        }
        else {
          val s = n.c.extend
          new Extension[Either[A, Either[B, C]], N] {
            type Type = Either[A, Either[B, s.Type]]
            def newValues(n : N) = Right(Right(s.newValues(n)))
            lazy val fun = up(f2)
            def f2 : Fun[Either[B, s.Type], Either[Either[B, C], N]] = TwoFun(
              LeftFun[B]() thenFun LeftFun(),
              s.fun thenFun TwoFun(
                  RightFun[C]() thenFun LeftFun(),
                  RightFun[N]()))
            val id = n.copy(c = s.id)
          }
        }
      }
    }
  }
  def combineNothing[B, C, N : Key](n: IdNode[Nothing, B, C]) : Extension[Either[Nothing, Either[B, C]], N] = new Extension[Either[Nothing, Either[B, C]], N] {
    type Type = Either[N, Either[B, C]]
    def newValues(n : N) = Left(n)
    val fun : Fun[Type, Either[Either[Nothing, Either[B, C]], N]] =
      TwoFun(RightFun(), CompFun(LeftFun[Either[Nothing, Either[B, C]]](), RightFun[Either[B, C]]()))
    val id : Id[Type] = n.copy(a = implicitly[Key[N]])
  }
}
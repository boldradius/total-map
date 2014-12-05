package com.boldradius

package object total {
  type AnyId = Id2[_, _, _]
  type TotalSuper[-K <: AnyId, V] = Total[V] {type Id >: K <: AnyId}
  type TotalSub[+K <: AnyId, V] = Total[V] {type Id <: K}
}

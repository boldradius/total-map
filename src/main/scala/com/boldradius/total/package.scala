package com.boldradius

package object total {
  type TotalSuper[-K <: AnyId, V] = Total[V] {type Id >: K <: AnyId}
  type TotalSub[+K <: AnyId, V] = Total[V] {type Id <: K}
  type TotalMap[K <: AnyId, V] = Total[V] {type Id = K}
}

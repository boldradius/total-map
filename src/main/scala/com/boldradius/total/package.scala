package com.boldradius

package object total {
  type TotalSuper[-K <: Id2[_, _, _], V] = Total[V] {type Id >: K <: Id2[_, _, _]}
  type TotalSub[+K, V] = Total[V] {type Id <: K}
  type TotalMap[K, V] = Total[V] {type Id = K}
}

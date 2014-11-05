package com.boldradius.total

import scala.math.BigInt

/**
 * Compact representation for a value of type [[K]] (where there exists a Key[K]).
 */
class Num[K] private(n: BigInt) {
  def value(implicit key: Key[K]) : K = Num.fromNum(n, key)
}
object Num {
  def apply[K](k : K)(implicit key: Key[K]) : Num[K] = new Num(toNum(k, key))

  /**
   * This numbering scheme is stable across K types that differ only at
   * Nothing leaves. Substituting an Either for a Unit would not or vice-versal
   * would lead to a number referring to a different node in the tree.
   */
  private def toNum[K](k : K, key: Key[K]) : BigInt = key match {
    case NoKey => k
    case UnitKey => 0
    case EitherKey(ka, kb) => k.fold(2 * toNum(_, ka), 2 * toNum(_, kb) + 1)
  }
  private def fromNum[K](n : BigInt, key: Key[K]) : K = key match {
    case NoKey => ??? // Will not happen when number is built with toNum with the same K
    case UnitKey => ()
    case EitherKey(ka, kb) =>
      if (n.testBit(0)) Right(fromNum(n >> 1, kb))
      else Left(fromNum(n >> 1, ka))
  }
}
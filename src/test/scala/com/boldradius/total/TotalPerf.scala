package com.boldradius.total

import org.scalameter.{Gen, PerformanceTest}

/**
 * Created by ppremont on 14-12-05.
 */
class TotalPerf extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(10000, 100000, 90000)
  val dummy: Gen[Unit] = Gen.single("")(Unit)

  performance of "Total" in {
    measure method "insert" in {
      using(sizes) in {s =>
        val added = Total.empty.insertAll(0 to s)
      }
    }
    val toSplit = Total.empty.insertAll(0 to 100000).total
    measure method "filter" in {
      using(dummy) in { _ =>
        toSplit.filter(_ % 4 == 0)
      }
    }
  }
}
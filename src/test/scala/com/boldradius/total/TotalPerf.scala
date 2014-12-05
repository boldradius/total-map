package com.boldradius.total

import org.scalameter.{Gen, PerformanceTest}

/**
 * Created by ppremont on 14-12-05.
 */
class TotalPerf extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(10000, 100000, 90000)

  performance of "Total" in {
    measure method "insert" in {
      using(sizes) in {s =>
        val added = Total.empty.insertAll(0 to s)
      }
    }
  }
}
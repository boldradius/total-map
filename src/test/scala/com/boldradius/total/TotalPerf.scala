package com.boldradius.total

import org.scalameter.{Gen, PerformanceTest}
import collection.breakOut

/**
 * Created by ppremont on 14-12-05.
 */
class TotalPerf extends PerformanceTest.Quickbenchmark {
  val sizes: Gen[Int] = Gen.range("size")(100000, 100000, 90000)
  val dummy: Gen[Unit] = Gen.single("")(Unit)

  performance of "Total" in {
    measure method "insert" in {
      using(sizes) in {s =>
        val added = Total.empty.insertAll(0 to s)
      }
    }
    measure method "insertInMap" in {
      using(sizes) in {s =>
        val added = (0 to s).zipWithIndex.map(_.swap).toMap
      }
    }
    measure method "insertInMapBreakout" in {
      using(sizes) in {s =>
        val added : Map[Int, Int] = (0 to s).zipWithIndex.map(_.swap)(breakOut)
      }
    }
    val toSplit = Total.empty.insertAll(0 to 100000).total
    measure method "filter" in {
      using(dummy) in { _ =>
        toSplit.filter(_ % 4 == 0)
      }
    }
    val toDiff1 = Total.empty.insertAll(0 to 100000).total
    val toDiff2 = toDiff1.filter(_ % 4 == 0)
    System.out.println(toDiff2.size)
    measure method "filter" in {
      using(dummy) in { _ =>
        toDiff1.difference(toDiff2)
      }
    }
    System.out.println(toDiff1.difference(toDiff2).size)
  }


  trait SyncedTotals[V] {
    val t1 : Total[V]
    val t2 : t1.Map[V]
    /*def insert(v: V) = new SyncedTotals[V] {
      val i = SyncedTotals.this.t1.allocate
      val t1 = SyncedTotals.this.t1.insertAt(i, v)
      val t2 = SyncedTotals.this.t2.insertAt(i, v)
    }*/
  }
  val s1 = new SyncedTotals[Int] { val t1 = TotalNothing; val t2 = TotalNothing }
  //s1.insert(10)
}
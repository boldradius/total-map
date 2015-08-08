import com.boldradius.total._

object Main {
  def main(a: Array[String]) : Unit = {

    //val added = Total.empty.insertAll(0 to 1000000)
    //val removed = added.total.remove(added.newIds.head)
    //println(removed.total.toStream.take(100).toList)

    println(List(100000, 1000000).map(size =>
      (size,
        benchmarkNanos(Total.empty.insertAll(0 to size))/size*1e6f,
        //benchmarkNanos((1 to size).map(i => (i, i)).toMap : Map[Int, Int]),
        benchmarkNanos((1 to size).map(i => (i, i))(scala.collection.breakOut) : Map[Int, Int])/size*1e6f
        )))
  }

  def benchmarkNanos(task: => Unit, warmupNanos : Long = 10*1000*1000*1000, runNanos : Long = 10*1000*1000*1000, runs: List[Long] = Nil) : Float = {
    val nanos = timeNanos(task)
    if (warmupNanos > 0) benchmarkNanos(task, warmupNanos - nanos, runNanos, runs)
    else if (runNanos > 0) benchmarkNanos(task, 0, runNanos - nanos, nanos :: runs)
    else (nanos :: runs).map(_.toFloat).average * 1e-9f
  }
  implicit class FloatSeqOps(v: Seq[Float]) {
    def average = v.sum / v.length
  }
  def timeNanos(task: => Unit) : Long = {
    val start = System.nanoTime()
    task
    System.nanoTime() - start
  }
}

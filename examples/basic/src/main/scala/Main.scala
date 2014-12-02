import com.boldradius.total._

object Main {
  def main(a: Array[String]) = {
    var a: Graph[_] = Graph[Nothing](Total.empty, Nil)
    (1 to 1000).foreach(_ => a = a.addNode) // TODO: Investigate performance/memory-usage, has trouble around 1M to 2M,

    /*
    var b: Relation[_] = Relation[Nothing](TotalNothing, Total.empty)
    (1 to 1000).foreach { _ =>
      // Add two new elements
      b = b.addElement.addElement
      // Remove one
      b = b.removeSomeElement
    }
    */


    val added = Total.empty.insertAll(0 to 1000000)
    val removed = added.total.remove(added.newIds.head)
    println(removed.total
      .toStream.take(100).toList)
  }
}

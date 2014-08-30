import com.boldradius.total._

object Main {
  def main(a: Array[String]) = {
    var a: Graph[_] = Graph[Nothing](NothingId, Nil)
    (1 to 1000).foreach(_ => a = a.addNode) // TODO: Investigate performance/memory-usage, has trouble around 1M to 2M,

    var b: Relation[_] = Relation[Nothing](TotalNothing, NothingId)
    (1 to 1000).foreach { _ =>
      // Add two new elements
      b = b.addElement.addElement
      // Remove one
      b = b.removeSomeElement
    }
  }
}

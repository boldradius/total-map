import com.boldradius.total._

object Main {
  def main(a: Array[String]) = {

    val added = Total.empty.insertAll(0 to 1000000)

    val removed = added.total.remove(added.newIds.head)

    println(removed.total
      .toStream.take(100).toList)

  }
}

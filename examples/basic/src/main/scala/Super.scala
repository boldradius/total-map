import com.boldradius.total._

trait Super {
  val set : Total[Unit]
  val subset : Total[Unit] {type Key <: set.Id} // Implements a primary key which is also a foreign key
  val superset : Total[Unit] {type Key >: set.Id}
}

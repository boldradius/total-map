import com.boldradius.total._

sealed trait Simple {
  val owners : Total[String]
  val cars : List[owners.Id]

  def newCarNewOwner(name: String) : Simple = {
    val inserted = owners.insert(name)
    Simple(inserted.total)(inserted.newId :: cars)
  }

  def deleteOwner(ownerId: owners.Id) : Simple = {
    val removal = owners.remove(ownerId)
    Simple(removal.total)(cars.flatMap(removal.filter(_)))
  }
}
object Simple {
  def apply(owners_ : Total[String])(cars_ : List[owners_.Id]) = new Simple {
    val owners : owners_.type = owners_
    val cars = cars_
  }
}

// This simple syntax does work yet: SI-5712 dependent constructor types
// sealed class Simple(val owners : Total[String])(val cars : List[owners.Id])


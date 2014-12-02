import com.boldradius.total._

// Consider changing to users and organisations
sealed trait Simple {
  val accounts : Total[Double]
  val customers : List[accounts.Id]

  def newCustomer = {
    val insert = accounts.insert(0.0)
    Simple(insert.total)(insert.newId :: customers) : Simple
  }

  def newCustomerExistingAccount(account: accounts.Id) =
    Simple(accounts)(account :: customers)

  def deleteAccount(accountId: accounts.Id) = {
    val removal = accounts.remove(accountId)
    Simple(removal.total)(customers.flatMap(removal.filter(_))) : Simple
  }
}
object Simple {
  def apply(accounts_ : Total[Double])(customers_ : List[accounts_.Id]) = new Simple {
    val accounts : accounts_.type = accounts_
    val customers = customers_
  }
}

// This simple syntax does work yet: SI-5712 dependent constructor types
// sealed class Simple(val set : Total[_, Unit])(val elements : List[set.Key]) {}


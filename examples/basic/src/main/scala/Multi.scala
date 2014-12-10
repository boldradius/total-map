import com.boldradius.total._

case class Country(name: String)
case class Address[+CountryId](country: CountryId)
case class Customer[+AddressId](address: AddressId)

abstract class Data {
  val countries : Total[Country]
  val addresses : Total[Address[countries.Id]]
  val customers : Total[Customer[addresses.Id]]
}
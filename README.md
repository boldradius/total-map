#total-map 0.2.1
===========

Library providing `Total`, a map-like data structure that assigns a value to every possible key.
It can be seen as a memoized function, but
it is designed to be updated as a data structure.

Insertion and deletion will add or remove keys, and must therefore produce total maps that have different key types.
The key type is therefore a type member of `Total` named `Key`.

Evolving the key types allows an application to **statically enforce referential integrity** between data structures:
every reference from one into the other is valid, there are no dangling references. This can prevent error conditions
such as 'post not found' or 'user not found'. The types themselves grow or shrink appropriately during the execution the application, 
forcing data structures indexed on these identifier types to be updated. Using `Total` allows an application to shift the burden of proof towards
the functions that update maps and away from functions that access them.

## Example:

Let's say you have an application that maintains a collection of users, 
and would to refer to users by their ids. You could do 
    case class UserId(v: Long)
    val map : Map[UserId, User] = ???
    val id : UserId = ???
but then we have no compiler enforced guarantee that any particular user id is defined in the map. 
A `UserId` may be a dangling reference within the map data structure.

If we want to prevent that, we can instead use a `Total` map.
Now the solution would look like this:
    val total : Total[User] = ???
    val id : total.Key = ???
where `A` is in type class `Key`.

Now we are guaranteed that the id, of type `total.Key`, has a corresponding `User` in `total`.

This way we can have data structures that enforce referential integrity. 
We can safely represent relations over dynamic sets.
This can eliminate one more class of bugs from our applications.

## Usage

Add a resolver and the library dependency to your sbt project:

    Resolver.bintrayRepo("boldradiussolutions", "maven")
    "com.boldradius" %% "total-map" % "0.2.1"


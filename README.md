#total-map
===========

Library providing `Total`, a map data structure that assigns a value to every possible key.
It is a total function, unlike `Map` which is a partial function. It can be seen as a memoized function, but
it is designed to be updated as a data structure.

Insertion and deletion will add or remove keys, and must therefore produce total maps that have different key types.
The key types are restricted to a `Key` type-class, and are designed to evolve at run-time as needed.

Evolving the key types allows an application to **statically enforce referential integrity** between data structures: that
every reference from one into the other is valid, and is not a dangling reference. This can prevent error conditions
such as 'post not found' or 'user not found'. The types themselves grow or shrink appropriately during the execution the application, 
forcing data structures indexed on these identifier types to be updated. Using `Total` allows an application to shift the burden of proof towards
the functions that update maps and away from functions that simply access it.

## Example:

Let's say you have an application that maintains a collection of users, 
and would like each user to be able refer to other users. You could do 
<pre><code>
case class UserId(v: Long)
Map[UserId, List[UserId]]
</code></pre>
but then we have no compiler enforced guarantee that the user ids in the list are defined in the map. 
So we could have `Map(UserId(1) -> List(UserId(2)), UserId(3) -> List(UserId(1)))`.
We can have dangling references within that data structure.

If we want to prevent that, we can instead use the `Key` type class and the `Total` map.
Now the solution would look like this:
<pre><code>
`Total[A, List[A]]`
</code></pre>where `A` is in type class `Key`.

Now every user id of type `A` has a corresponding list of user ids (of type `A`).
 
To insert a user in this structure, we generate a new user id type `B` that has one more potential value than `A`.  (see the code in the `example/basic` directory)


This way we can have data structures that enforce referential integrity. 
We can safely represent relations over dynamic sets.
This can eliminate one more class of bugs from our applications.

An interesting fact is that `Total` is contravariant in the key type. This is consistent with is role as a function. So any instance can be
treated as a restricted view: a total map with a smaller key type (key subtype).
 

<!---
# How it works
The type class `Key[A]` admits types `A` that are meant to be used as identifiers. 
These are extensible disjunctions of Unit (trees of `Either`/`Unit`/`Nothing`). 
An insertion function is provided that goes from a type in this class to one that is a 
similar disjunction but has one more alternative.

Note that some types in `Key` are not be extensible into super types The Unit type is such an example. This library also provides a
more restrictive type-class `Id` that only contains types that can be extended into super types.
--->


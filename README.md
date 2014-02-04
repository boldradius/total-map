typeSafeIds
===========

Library providing type-safe identifiers to enforce referential integrity.
The types themselves grow or shrink appropriately during the execution the application, 
forcing data structures indexed on these identifier types to be updated.

The type class `Id[A]` admits types `A` that are meant to be used as identifiers. 
These are extensible disjunctions of Unit (trees of `Either`/`Unit`/`Nothing`). 
An insertion function is provided that goes from a type in this class to one that is a 
similar disjunction but has one more alternative.

Here is where it would be useful:

Let's say you have an application that maintains a collection of Users, 
and would like each User to be able refer to each other. You could do 
<pre><code>
case class UserId(v: Long)
Map[UserId, List[UserId]]
</code></pre>
but then we have no compiler enforced guarantee that the `UserId`s in the `List` are defined in the `Map`. 
So we could have `Map(UserId(1) -> List(UserId(3)), UserId(3) -> List(UserId(2)))`.
We can have dangling references within that data structure.

If we want to prevent that, we can use the `Id` type class instead, along with an 
accompanying map-like data structure called `Total[K, V]`. Now the solution would look like this:
<pre><code>
`Total[A, List[A]]`
</code></pre>where `A` is in type class `Id`,
`Total` is a total map, so every key in the key type has a corresponding value in the map. 
When we add alternatives to a type in `Id`, we obtain a way to update any `Total`
collections that have that type as the key.

This way we can have data structures that enforce referential integrity. 
We can safely represent relations over dynamic sets.
This can eliminate one more class of bugs from our applications.

***
This library has yet to be used in production, and might be difficult to use on a large scale. 
Hopefully we'll have larger usage examples soon.
***

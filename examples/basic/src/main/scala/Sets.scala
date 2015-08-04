import com.boldradius.total._

trait Sets {
  val set : Total[Unit]
  val subset : set.Sub[Unit] // Implements a primary key which is also a foreign key
  val superset : set.Super[Unit]
  val sameSet : set.Map[Unit]
  //val disjointSet : TotalSub[set.Comp, Unit]
}

object Sets {
  def apply(s: Total[Unit])(sub: s.Sub[Unit], sup: s.Super[Unit], same: s.Map[Unit]) = new Sets {
    val set : s.type = s
    val subset : set.Sub[Unit] = sub
    val superset : set.Super[Unit] = sup
    val sameSet : set.Map[Unit] = same
  }

  def insert(s : Sets) : Sets = {
    val id = s.set.allocate
    val s1 : s.set.Super[Unit] = s.set.insertAt(id, ())
    //Sets(s1)(???, ???, ???)
    Sets(s1)(???, ???, s.sameSet.insertAt(id, ()))
    //Sets(s1)(s.subset.insertAt(id, ()), ???, s.sameSet.insertAt(id, ()))
    // We want to be able to non-destructively insert that into subset (or sameSet)

    // We need to know that it is not already in subset.
    // We could record that was in the complement of s.set
    // Could we deduce with types that it is then also in the complement of s.subset, and can therefore be inserted non-desructively ?

    // The ability to non-destructively insert a particular element, instead of relying on a particular built-in allocation policy is strictly more powerful and
    // enables the maintenance of constrains between sets (between their Id types).
    // Allocation of an element in the complement become a prior step on the Total. If the intent is to insert into constrained Totals, allocation
    // should be performed on the largest Total, guaranteeing that the allocated if is also in complement of the smaller totals
    // The reverse applies to removal
    // After we've inserted to constrained totals, are they still constrained ? Or, at least, can be do so in a way that keeps them constrained ?
  }
}

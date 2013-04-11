package id

/**
 * Id is a closed typeclass for trees of Either, Unit and Nothing types. We will use types in the
 * Id typeclass as identifiers. 
 */
sealed trait Id[A]
case class IdNothing() extends Id[Nothing]
case class IdUnit() extends Id[Unit]
case class IdEither[B, C](b: Id[B], c: Id[C]) extends Id[Either[B, C]]

object Id {
  implicit val nothingId : Id[Nothing] = IdNothing()
  implicit val unitId : Id[Unit] = IdUnit()
  implicit def eitherId[B : Id, C: Id] : Id[Either[B, C]] = IdEither(implicitly[Id[B]], implicitly[Id[C]])
}

object IdOps {
  /** An ordering for any type in the Id typeclass. This makes it possible to use Map. */
  def <[A : Id](a1: A, a2: A) : Boolean =
    implicitly[Id[A]] match {
      case IdNothing() => false
      case IdUnit() => false
      case IdEither(b, c) => 
        a1.fold(e1l => a2.fold(e2l => <(e1l, e2l)(b), _ => true), 
            e1r => a2.fold(_ => false, e2r => <(e1r, e2r)(c)))
    }
}


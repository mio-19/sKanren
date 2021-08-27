package skanren

import scala.collection.immutable.HashMap

sealed trait Hole[T]

final class UniqueHole[T](identifier: Symbol) extends Hole[T]

final case class Substitution[T](hole: Hole[T], value: T)

type SubstitutionAny = Substitution[Any]

type Substitutions = HashMap[Hole[_], _]

implicit class SubstitutionsOps(subst: Substitutions) {
  def walk[T <: Unifiable](x: T): T = x.matchHole match {
    case Some(hole) => subst.get(hole) match {
      case Some(next) => this.walk(next.asInstanceOf[T])
      case None => x
    }
    case None => x
  }
}

object Substitutions {
  def of[T](hole: Hole[T], value: T): Substitutions = HashMap((hole, value))

  def unchecked[T, U](hole: Hole[T], value: U): Substitutions = of[Any](hole.asInstanceOf[Hole[Any]], value)
}

trait Unifiable {
  type T <: Unifiable
  //implicit protected val ev: this.type <:< T
  implicit protected val ev: this.type =:= T

  def unify(subst: Substitutions, other: T): Option[Substitutions] =
    (subst.walk(this), subst.walk(other)) match {
      case (self, other) =>
        (self.matchHole, other.matchHole) match {
          case (Some(self), None) => Some(Substitutions.unchecked(self, other))
          case (None, Some(other)) => Some(Substitutions.unchecked(other, self))
          case (Some(self), Some(other)) => Some(Substitutions.unchecked(self, other))
          case (None, None) => this.unifyConcrete(subst, other)
        }
    }

  protected def unifyConcrete(subst: Substitutions, other: T): Option[Substitutions] = throw new UnsupportedOperationException()

  def matchHole: Option[Hole[T]]
}

implicit class UnifiablePatternMatching[T <: Unifiable](x: T) {
  def mat[R](clauses: T => R): R = ???
}

sealed trait Holeable[A] extends Unifiable {
  override type T = Holeable[A]
  override implicit protected val ev = implicitly
}

final case class HoleablePure[T](x: T) extends Holeable[T] {
  override def matchHole = None
}

final case class HoleableHole[T](x: Hole[Holeable[T]]) extends Holeable[T] {
  override def matchHole = Some(x)
}

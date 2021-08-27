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
  type T >: this.type <: Unifiable

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
  // well, it's hard to re-use scala pattern matching syntax
  def mat[R](clauses: T => R): R = ???
}

trait ConcreteUnifiable {
  type T >: this.type <: ConcreteUnifiable

  def unifyConcrete(subst: Substitutions, other: T): Option[Substitutions]
}

sealed trait Holeable[A <: ConcreteUnifiable] extends Unifiable {
  override type T = Holeable[A]
}

final case class HoleablePure[T <: ConcreteUnifiable](x: T) extends Holeable[T] {
  override def matchHole = None

  override def unifyConcrete(subst: Substitutions, other: Holeable[T]) = other match {
    case HoleablePure(other: T) => x.unifyConcrete(subst, other.asInstanceOf[x.T])
    case HoleableHole(_) => throw new IllegalStateException()
  }
}

final case class HoleableHole[T <: ConcreteUnifiable](x: Hole[Holeable[T]]) extends Holeable[T] {
  override def matchHole = Some(x)
}

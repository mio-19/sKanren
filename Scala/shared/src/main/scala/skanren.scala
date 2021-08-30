package skanren

import scala.collection.immutable.HashMap
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet, ParVector}

sealed trait Hole[T]

final class UniqueHole[T](identifier: Symbol) extends Hole[T]

final case class Substitution[T](hole: Hole[T], value: T)

type SubstitutionAny = Substitution[Any]

type SubstitutionStore = ParHashMap[Hole[_], _]

type NegSubstitution = ParVector /*or*/ [SubstitutionAny]

type NegSubstitutionStore = ParVector /*and*/ [NegSubstitution]

implicit class SubstitutionStoreOps(subst: SubstitutionStore) {
  def walk[T <: Unifiable](x: T): T = x.matchHole match {
    case Some(hole) => subst.get(hole) match {
      case Some(next) => this.walk(next.asInstanceOf[T])
      case None => x
    }
    case None => x
  }

  def add(x: GoalUnify[_]): Option[SubstitutionStore] = x match {
    case GoalUnify(a, b) => Unifiable.unify(subst, a, b)
  }

  def diff(sub: SubstitutionStore): ParVector[SubstitutionAny] = ???

  def insertUnchecked[T, U](hole: Hole[T], value: U): SubstitutionStore = ???
}

object NegSubstitution {
  def add1(subst: SubstitutionStore, x: GoalNegUnify[_]): Option[NegSubstitution] = x match {
    case GoalNegUnify(a, b) => a.unify(subst, b.asInstanceOf[a.T]) match {
      case None => Some(ParVector())
      case Some(newsubst) => {
        val diff = subst.diff(newsubst)
        if (diff.isEmpty) None else Some(diff)
      }
    }
  }
}

implicit class NegSubstitutionStoreOps(negs: NegSubstitutionStore) {
  def addAndNormalize(subst: SubstitutionStore, xs: ParVector[GoalNegUnify[_]]): Option[NegSubstitutionStore] = ???
}

trait Unifiable {
  type T >: this.type <: Unifiable

  def unify(subst: SubstitutionStore, other: T): Option[SubstitutionStore] =
    (subst.walk(this), subst.walk(other)) match {
      case (self, other) =>
        (self.matchHole, other.matchHole) match {
          case (Some(self), None) => Some(subst.insertUnchecked(self, other))
          case (None, Some(other)) => Some(subst.insertUnchecked(other, self))
          case (Some(self), Some(other)) => Some(subst.insertUnchecked(self, other))
          case (None, None) => this.unifyConcrete(subst, other)
        }
    }

  protected def unifyConcrete(subst: SubstitutionStore, other: T): Option[SubstitutionStore] = throw new UnsupportedOperationException()

  def matchHole: Option[Hole[T]]

  final def ==(other: T): Goal = GoalUnify(this, other)

  final def !=(other: T): Goal = GoalNegUnify(this, other)
}

object Unifiable {
  def unify[T <: Unifiable](subst: SubstitutionStore, a: T, b: T): Option[SubstitutionStore] = a.unify(subst, b.asInstanceOf[a.T])

  def unify[T <: Unifiable, U <: Unifiable](subst: SubstitutionStore, a: T, b: T, x: T, y: T): Option[SubstitutionStore] = for {
    subst <- unify(subst, a, b)
    subst <- unify(subst, x, y)
  } yield subst

  def unify[A <: Unifiable, B <: Unifiable](subst: SubstitutionStore, a: (A, B), b: (A, B)): Option[SubstitutionStore] = for {
    subst <- unify(subst, a._1, b._1)
    subst <- unify(subst, a._2, b._2)
  } yield subst

  def unify[A <: Unifiable, B <: Unifiable, C <: Unifiable](subst: SubstitutionStore, a: (A, B, C), b: (A, B, C)): Option[SubstitutionStore] = for {
    subst <- unify(subst, a._1, b._1)
    subst <- unify(subst, a._2, b._2)
    subst <- unify(subst, a._3, b._3)
  } yield subst
}

/*
implicit class UnifiablePatternMatching[T <: Unifiable](x: T) {
  // well, it's hard to re-use scala pattern matching syntax
  def mat[R](clauses: T => R): R = ???
}
*/

final case class Unifying[T](f: SubstitutionStore => Option[(SubstitutionStore, T)])

trait Extractor[T, U] {
  def unapplyo(x: T): Unifying[U]
}

trait ConcreteUnifiable extends Unifiable {
  override type T >: this.type <: ConcreteUnifiable

  override def unifyConcrete(subst: SubstitutionStore, other: T): Option[SubstitutionStore] = throw new UnsupportedOperationException("Must implement unifyConcrete")

  final override def unify(subst: SubstitutionStore, other: T): Option[SubstitutionStore] = this.unifyConcrete(subst, other)

  final override def matchHole: Option[Hole[T]] = None
}

sealed trait Holeable[A <: ConcreteUnifiable] extends Unifiable {
  override type T = Holeable[A]
}

final case class HoleablePure[T <: ConcreteUnifiable](x: T) extends Holeable[T] {
  override def matchHole = None

  override def unifyConcrete(subst: SubstitutionStore, other: Holeable[T]) = other match {
    case HoleablePure(other: T) => x.unifyConcrete(subst, other.asInstanceOf[x.T])
    case HoleableHole(_) => throw new IllegalStateException()
  }
}

final case class HoleableHole[T <: ConcreteUnifiable](x: Hole[Holeable[T]]) extends Holeable[T] {
  override def matchHole = Some(x)
}

sealed trait Goal {
  def &&(other: Goal) = GoalConde(ParVector(ParVector(this, other)))

  def ||(other: Goal) = GoalConde(ParVector(ParVector(this), ParVector(other)))
}

object Goal {
  def apply(x: => Goal) = new GoalDelay(x)
}

sealed trait SimpleGoal extends Goal

final case class GoalUnify[T <: Unifiable](x: T, y: T) extends SimpleGoal

final case class GoalNegUnify[T <: Unifiable](x: T, y: T) extends SimpleGoal

final case class GoalType(t: Class[_], x: Unifiable) extends SimpleGoal

final case class GoalNegType(t: Class[_], x: Unifiable) extends SimpleGoal

final case class GoalConde(clauses: ParVector[ParVector[Goal]]) extends Goal {
  def &&(other: GoalConde): GoalConde = GoalConde(for {
    a <- this.clauses
    b <- other.clauses
  } yield a ++ b)

  def ||(other: GoalConde): GoalConde = GoalConde(this.clauses ++ other.clauses)
}

final class GoalDelay(x: => Goal) extends Goal {
  lazy val get: Goal = x
}

final case class TypeStore(xs: ParHashSet[Hole[_]]) {
  def addAndNormalize(subst: SubstitutionStore, news: ParVector[Hole[_]]) = ???
}

final case class NegTypeStore(xs: ParHashSet[Hole[_]]) {
  def addAndNormalize(subst: SubstitutionStore, news: ParVector[Hole[_]]) = ???
}

final case class Store(eq: SubstitutionStore, notEq: NegSubstitutionStore, typ: TypeStore, notTyp: NegTypeStore)

final case class Universe(store: Store, goals: ParVector[ParVector[Goal]])

type State = ParVector[Universe]

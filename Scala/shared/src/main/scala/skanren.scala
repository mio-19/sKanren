package skanren


import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet, ParVector, ParIterable}
import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

private val EMPTY_SYMBOL = Symbol("")

final class Hole[T](identifier: Symbol) {
  def this() = {
    this(EMPTY_SYMBOL)
  }
}

type SubstitutionStore = ParHashMap[Hole[_], _]

implicit class SubstitutionStoreOps(subst: SubstitutionStore) {

  private def get[T](x: Hole[T]): Option[T] = subst.get(x).asInstanceOf[Option[T]]

  def walk[T](x: T)(implicit unifier: Unifier[T]): T = (for {
    hole <- unifier.matchHole(x)
    next <- this.get(hole)
  } yield this.walk(next)).getOrElse(x)
}

object SubstitutionStore {
  inline def walk[T](x: T)(implicit unifier: Unifier[T]): Unifying[T] = subst => Some((subst, subst.walk(x)))

  inline def addEntry[T](h: Hole[T], x: T): Unifying[Unit] =
    subst => Some((if (subst.contains(h)) throw new IllegalArgumentException() else subst.updated(h, x)), ())
}

type Unifying[T] = SubstitutionStore => Option[(SubstitutionStore, T)]

implicit class UnifyingOps[A](f: Unifying[A]) {
  def flatMap[B](x: A => Unifying[B]): Unifying[B] = st => f(st).flatMap({ case (st, a) => x(a)(st) })

  def map[B](x: A => B): Unifying[B] = st => f(st).map({ case (st, a) => (st, x(a)) })
}

object Unifying {
  def fail[A]: Unifying[A] = st => None

  def pure[A](x: A): Unifying[A] = st => Some((st, x))

  def guard(x: Boolean): Unifying[Unit] = if (x) Unifying.pure(()) else Unifying.fail
}

trait Unifier[T] {
  final implicit val thisUnifier: Unifier[T] = this

  def unify(self: T, other: T): Unifying[Unit] = for {
    self <- SubstitutionStore.walk(self)
    other <- SubstitutionStore.walk(other)
    _ <- (this.matchHole(self), this.matchHole(other)) match {
      case (Some(selfHole), _) => SubstitutionStore.addEntry(selfHole, other)
      case (None, Some(otherHole)) => SubstitutionStore.addEntry(otherHole, self)
      case (None, None) => this.concreteUnify(self, other)
    }
  } yield ()

  protected def concreteUnify(self: T, other: T): Unifying[Unit] = throw new UnsupportedOperationException("concreteUnify is not implemented")

  def matchHole(x: T): Option[Hole[T]] = ???
}

implicit class UnifierOps[T](x: T)(implicit unifier: Unifier[T]) {
  // todo
}

trait ConcreteUnifier[T] extends Unifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = throw new UnsupportedOperationException("concreteUnify is required")

  final override def matchHole(x: T): Option[Hole[T]] = None
}

trait ConcreteAtomUnifier[T] extends ConcreteUnifier[T] {
  override def concreteUnify(self: T, other: T): Unifying[Unit] = Unifying.guard(self == other)
}

implicit object SymbolUnifier extends ConcreteAtomUnifier[Symbol]

implicit object UnitUnifier extends ConcreteAtomUnifier[Unit]

implicit object StringUnifier extends ConcreteAtomUnifier[String]

implicit object BooleanUnifier extends ConcreteAtomUnifier[Boolean]

sealed trait Goal {
  def &&(other: Goal) = GoalConde(ParVector(ParVector(this, other)))

  def ||(other: Goal) = GoalConde(ParVector(ParVector(this), ParVector(other)))

  def asConde(depth: Int): GoalConde = GoalConde(ParVector(ParVector(this)))
}

object Goal {
  def apply(x: => Goal): Goal = GoalDelay(x)
}

sealed trait SimpleGoal extends Goal {
  def tag: Int
}

object SimpleGoalTags {
  val GoalUnify = 0
  val GoalNegUnify = 1
  val GoalType = 2
  val GoalNegType = 3
}

final case class GoalUnify[T](x: T, y: T)(implicit unifier: Unifier[T]) extends SimpleGoal {
  override def tag = SimpleGoalTags.GoalUnify

}

final case class GoalNegUnify[T](x: T, y: T)(implicit unifier: Unifier[T]) extends SimpleGoal {
  override def tag = SimpleGoalTags.GoalNegUnify

}

final case class GoalType[T](hole: Hole[T], cls: ClassTag[_])(implicit unifier: Unifier[T]) extends SimpleGoal {
  override def tag = SimpleGoalTags.GoalType

}

final case class GoalNegType[T](hole: Hole[T], cls: ClassTag[_])(implicit unifier: Unifier[T]) extends SimpleGoal {
  override def tag = SimpleGoalTags.GoalNegType

}

final class GoalDelay(x: => Goal) extends Goal {
  lazy val get: Goal = x
}

final case class GoalConde(clauses: ParVector[ParVector[Goal]]) extends Goal {
  def &&(other: GoalConde): GoalConde = GoalConde(for {
    a <- this.clauses
    b <- other.clauses
  } yield a ++ b)

  def ||(other: GoalConde): GoalConde = GoalConde(this.clauses ++ other.clauses)


  override def asConde(depth: Int): GoalConde = if (depth <= 0) super.asConde(depth) else {
    val newDepth = depth - 1
    if (clauses.isEmpty)
      this
    else
      clauses.map(block => if (block.isEmpty) GoalConde.success else block.map(_.asConde(newDepth)).reduce(_ && _)).reduce(_ || _)
  }
}

object GoalConde {
  // xs can't be empty
  def asConde(xs: ParVector[Goal], depth: Int): GoalConde = {
    val newDepth = depth - 1
    xs.map(_.asConde(depth)).reduce(_ && _)
  }

  val success: GoalConde = GoalConde(ParVector(ParVector()))
  val failure: GoalConde = GoalConde(ParVector())

  def ands(xs: ParVector[Goal]) = GoalConde(ParVector(xs))
}

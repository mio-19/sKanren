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

type SubstitutionStore = ParHashMap[Hole[_], SubstitutionElement[_]]

final case class SubstitutionElement[T](x: T, unifier: Unifier[T]) {
}

final case class Substitution[T](hole: Hole[T], x: T, unifier: Unifier[T]) {
}

object Substitution {
  def unchecked[A](hole: Hole[_], x: A, unifier: Unifier[_]) =
    Substitution[Any](hole.asInstanceOf[Hole[Any]], x, unifier.asInstanceOf[Unifier[Any]])
}

implicit class SubstitutionStoreOps(subst: SubstitutionStore) {

  private def get[T](x: Hole[T]): Option[T] = subst.get(x).map(x => x.x.asInstanceOf[T])

  def walk[T](x: T)(implicit unifier: Unifier[T]): T = (for {
    hole <- unifier.matchHole(x)
    next <- this.get(hole)
  } yield this.walk(next)).getOrElse(x)

  def add[T](goal: GoalUnify[T]): Option[SubstitutionStore] = goal.unifier.unify(goal.x, goal.y)(subst).map(_._1)

  def adds(xs: ParVector[GoalUnify[_]]): Option[SubstitutionStore] =
    xs.foldLeft(Some(subst): Option[SubstitutionStore])((s, x) => s.flatMap(_.add(x)))

  def diff(sub: SubstitutionStore): ParVector[Substitution[_]] = ParVector() ++ sub.filter((k, v) => subst.get(k) match {
    case Some(raw) => {
      assert(v == raw)
      false
    }
    case None => true
  }).map((k, v) => Substitution.unchecked(k, v.x, v.unifier))
}

object SubstitutionStore {
  inline def walk[T](x: T)(implicit unifier: Unifier[T]): Unifying[T] = subst => Some((subst, subst.walk(x)))

  inline def addEntry[T](h: Hole[T], x: T)(implicit unifier: Unifier[T]): Unifying[Unit] =
    subst => Some((if (subst.contains(h)) throw new IllegalArgumentException() else subst.updated(h, SubstitutionElement(x, unifier))), ())
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

implicit class UnifierOps[T](self: T)(implicit unifier: Unifier[T]) {
  def ===(other: T): GoalUnify[T] = GoalUnify(self, other)

  def =/=(other: T): GoalNegUnify[T] = GoalNegUnify(self, other)
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

final case class GoalUnify[T](x: T, y: T)(implicit ev: Unifier[T]) extends SimpleGoal {
  val unifier = ev

  override def tag = SimpleGoalTags.GoalUnify

}

final case class GoalNegUnify[T](x: T, y: T)(implicit ev: Unifier[T]) extends SimpleGoal {
  val unifier = ev

  override def tag = SimpleGoalTags.GoalNegUnify

}

final case class GoalType[T](hole: Hole[T], cls: ClassTag[_])(implicit ev: Unifier[T]) extends SimpleGoal {
  val unifier = ev

  override def tag = SimpleGoalTags.GoalType

}

final case class GoalNegType[T](hole: Hole[T], cls: ClassTag[_])(implicit ev: Unifier[T]) extends SimpleGoal {
  val unifier = ev

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

type NegSubstitution = ParVector /*or*/ [Substitution[_]]

type NegSubstitutionStore = ParVector /*and*/ [NegSubstitution]

implicit class NegSubstitutionStoreOps(negs: NegSubstitutionStore) {
  /*
  def addAndNormalize(subst: SubstitutionStore, xs: ParVector[GoalNegUnify[_]]): Option[NegSubstitutionStore] = {
    val newNegs = (xs.map(NegSubstitutionStoreOps.run(subst, _)).flatten) ++ (negs.map(NegSubstitutionStoreOps.run(subst, _)).flatten)
    if (newNegs.exists(_.isEmpty)) None else Some(newNegs)
  }
  */
}

object NegSubstitutionStoreOps {
  private def transverse[T](xs: ParVector[Option[T]]): Option[ParVector[T]] = try {
    Some(xs.map(_.get))
  } catch {
    case _: java.util.NoSuchElementException => None
  }

  /*
  // None means success, Some(ParVector()) means failure.
  // unchecked
  private def run[T](subst: SubstitutionStore, x: Unifiable, y: Unifiable): Option[NegSubstitution] = Unifiable.unify(subst, x, y) match {
    case None => None
    case Some(newst) => Some(subst.diff(newst))
  }

  // None means success, Some(ParVector()) means failure.
  private def run(subst: SubstitutionStore, goal: GoalNegUnify[_]): Option[NegSubstitution] = this.run(subst, goal.x, goal.y)

  // None means success, Some(ParVector()) means failure.
  private def run(subst: SubstitutionStore, element: Substitution[_]): Option[NegSubstitution] = element match {
    case Substitution(hole, x) => subst.get(hole) match {
      case Some(next) => this.run(subst, next, x)
      case None => Some(ParVector(element))
    }
  }

  // None means success, Some(ParVector()) means failure.
  private def run(subst: SubstitutionStore, x: NegSubstitution): Option[NegSubstitution] = transverse(x.map(this.run(subst, _))).map(_.flatten)
  */
}

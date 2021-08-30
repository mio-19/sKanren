package skanren

import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet, ParVector}
import scala.collection.parallel.CollectionConverters._

private val EMPTY_SYMBOL = Symbol("")

final class Hole[T](identifier: Symbol) {
  def this() = {
    this(EMPTY_SYMBOL)
  }
}

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

  def adds(xs: ParVector[GoalUnify[_]]): Option[SubstitutionStore] = ???

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

trait HasHole[T] {
  def hole(x: Hole[T]): T
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

  def asConde(depth: Int): GoalConde = GoalConde(ParVector(ParVector(this)))
}

object Goal {
  def apply(x: => Goal) = new GoalDelay(x)
}

sealed trait SimpleGoal extends Goal {
  def tag: Int
}

object GoalUnify {
  val tag = 0

  def unchecked[T <: Unifiable, U <: Unifiable](x: T, y: U) = GoalUnify[Unifiable](x, y)
}

final case class GoalUnify[T <: Unifiable](x: T, y: T) extends SimpleGoal {
  override def tag = GoalUnify.tag
}

object GoalNegUnify {
  val tag = 1
}

final case class GoalNegUnify[T <: Unifiable](x: T, y: T) extends SimpleGoal {
  override def tag = GoalNegUnify.tag
}

object GoalType {
  val tag = 2
}

final case class GoalType(t: Class[_], x: Unifiable) extends SimpleGoal {
  override def tag = GoalType.tag
}

object GoalNegType {
  val tag = 3
}

final case class GoalNegType(t: Class[_], x: Unifiable) extends SimpleGoal {
  override def tag = GoalNegType.tag
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

  val success: GoalConde = GoalConde(ParVector())

  def ands(xs: ParVector[Goal]) = GoalConde(ParVector(xs))
}

final class GoalDelay(x: => Goal) extends Goal {
  lazy val get: Goal = x

  override def asConde(depth: Int): GoalConde = if (depth <= 0) super.asConde(depth) else get.asConde(depth - 1)
}

final case class TypeStore(xs: ParHashSet[Hole[_]]) {
  def addAndNormalize(subst: SubstitutionStore, news: ParVector[GoalType]): Option[TypeStore] = ???
}

final case class NegTypeStore(xs: ParHashSet[Hole[_]]) {
  def addAndNormalize(subst: SubstitutionStore, news: ParVector[GoalNegType]): Option[NegTypeStore] = ???
}

final case class Store(eq: SubstitutionStore, notEq: NegSubstitutionStore, typ: TypeStore, notTyp: NegTypeStore) {
  def addSimpleGoals(goals: ParVector[SimpleGoal]): Option[Store] = {
    val all = goals.groupBy(_.tag)
    assert(all.size == 4)
    val eqGoals = all(GoalUnify.tag).map(_.asInstanceOf[GoalUnify[_]])
    val notEqGoals = all(GoalNegUnify.tag).map(_.asInstanceOf[GoalNegUnify[_]])
    val typGoals = all(GoalType.tag).map(_.asInstanceOf[GoalType])
    val notTypGoals = all(GoalNegType.tag).map(_.asInstanceOf[GoalNegType])
    for {
      eq0 <- eq.adds(eqGoals)
      notEq0 <- notEq.addAndNormalize(eq0, notEqGoals)
      typ0 <- typ.addAndNormalize(eq0, typGoals)
      notTyp0 <- notTyp.addAndNormalize(eq0, notTypGoals)
    } yield Store(eq0, notEq0, typ0, notTyp0)
  }
}

private val DEFAULT_DEPTH = 5

final case class Universe(store: Store, goals: ParVector[Goal]) {
  def step: ParVector[Universe] = if (goals.isEmpty) ParVector(this) else {
    val (simplesRaw, restGoals) = goals.partition(_.isInstanceOf[SimpleGoal])
    val simples = simplesRaw.map(_.asInstanceOf[SimpleGoal])
    if (restGoals.isEmpty)
      store.addSimpleGoals(simples) match {
        case None => ParVector()
        case Some(newstore) => ParVector(Universe.pure(newstore))
      }
    else
      store.addSimpleGoals(simples).map(newstore => {
        val GoalConde(conde) = GoalConde.asConde(restGoals, DEFAULT_DEPTH)
        conde.map(block => Universe(newstore, block))
      }).getOrElse(ParVector())
  }

  def isNormal: Boolean = goals.isEmpty

  def getNormal: Store = if (goals.isEmpty) store else throw new IllegalAccessException()
}

object Universe {
  def pure(x: Store): Universe = Universe(x, ParVector())
}

type State = ParVector[Universe]

implicit class StateOps(st: State) {
  def step: Option[State] = if (st.isEmpty) None else Some(st.map(_.step).flatten)

  def run1Vec: Option[(Vector[Store], State)] = {
    val (normals, newst) = st.partition(_.isNormal)
    if (normals.isEmpty) newst.step.flatMap(_.run1Vec) else Some((normals.map(_.getNormal).seq, newst))
  }

  def run1: Option[(Store, State)] = for {
    (normals, newst) <- this.run1Vec
  } yield (normals.head, normals.par.tail.map(Universe.pure(_)) ++ newst)

  def runAll: Vector[Store] = st.run1Vec match {
    case Some(normals, newst) => normals ++ newst.runAll
    case None => Vector()
  }
}

final case class Logic[T](goals: ParVector[Goal], x: T) {
  def map[U](f: T => U): Logic[U] = Logic(goals, f(x))

  def flatMap[U](f: T => Logic[U]): Logic[U] = {
    val res = f(x)
    Logic(goals ++ res.goals, res.x)
  }

  def or(other: Logic[T])(implicit ev: HasHole[T], ev1: T <:< Unifiable): Logic[T] = {
    val v = ev.hole(new Hole())
    Logic(ParVector(GoalConde(ParVector(GoalUnify.unchecked(v, this.x) +: this.goals, GoalUnify.unchecked(v, other.x) +: other.goals))), v)
  }
}

object Logic {
  private val emptyGoals: ParVector[Goal] = ParVector()

  def pure[T](x: T): Logic[T] = Logic(emptyGoals, x)

  def apply[T](x: T): Logic[T] = Logic(emptyGoals, x)

  def apply(x: Goal): Logic[Unit] = Logic(ParVector(x), ())

  def create[T, U](x: Hole[T] => Logic[U]): Logic[U] = x(new Hole())

  def create[T](x: T => Goal)(implicit ev: HasHole[T]): Logic[T] = {
    val h = ev.hole(new Hole())
    Logic(ParVector(x(h)), h)
  }
}

trait LogicExtractor[T, U] {
  def unapplyo(x: T): Logic[U]
}

/*
final case class Unifying[T](f: SubstitutionStore => Option[(SubstitutionStore, T)])

trait UnifyingExtractor[T, U] {
  def unapplyo(x: T): Unifying[U]
}
*/
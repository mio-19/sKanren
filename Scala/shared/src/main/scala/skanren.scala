package skanren

import scala.collection.immutable.HashMap
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LTT
import izumi.reflect.Tag

def typeOf[T](implicit ev: Tag[T]): LightTypeTag = ev.tag

final case class Unify(x: Unifiable, y: Unifiable) {
  def apply(context: UnifyContext): UnifyResult = x.unify(context, y)
}

final case class UnifyNormalForm(x: Hole, y: Unifiable)

type UnifyContext = HashMap[Hole, Unifiable]

implicit class UnifyContextImpl(ctx: UnifyContext) {
  def add(x: UnifyNormalForm): UnifyContext = x match {
    case UnifyNormalForm(x, y) => if (ctx.contains(x)) throw new IllegalArgumentException() else ctx.updated(x, y)
  }

  def add(xs: List[UnifyNormalForm]): UnifyContext = xs.foldLeft(ctx)((x, y) => x.add(y))
}

trait Unifiable {
  def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult

  final def unify(context: UnifyContext, other: Unifiable): UnifyResult = (this, other) match {
    case (self: Hole, other: Hole) => (self.walkOption(context), other.walkOption(context)) match {
      case (Some(self), Some(other)) => self.unify(context, other)
      case (Some(self), None) => self.unify(context, other)
      case (None, Some(other)) => self.unify(context, other)
      case (None, None) => Some(List(UnifyNormalForm(self, other)))
    }
    case (self: Hole, other) => self.walkOption(context) match {
      case Some(self) => other.unify(context, self)
      case None => Some(List(UnifyNormalForm(self, other)))
    }
    case (self, other: Hole) => other.unify(context, self)
    case (self, other) => self.impl_unify(context, other)
  }

  final def unify(context: UnifyContext, other: Unifiable, normal: UnifyNormalForm): UnifyResult = this.unify(context.add(normal), other).map(normal :: _)

  final def unify(context: UnifyContext, other: Unifiable, normal: UnifyResult): UnifyResult = normal.flatMap(normal => this.unify(context.add(normal), other).map(normal ++ _))

  final def unify(context: UnifyContext, other: Unifiable, x: Unifiable, y: Unifiable): UnifyResult = this.unify(context, other, x.unify(context, y))
}

type UnifyResult = Option[List[UnifyNormalForm]]

val UnifyResultFailure = None

final case class Hole(identifier: Symbol) extends Unifiable {
  def walkOption(context: UnifyContext): Option[Unifiable] = context.get(this) match {
    case Some(next: Hole) => Some(next.walk(context))
    case Some(next) => Some(next)
    case None => None
  }

  def walk(context: UnifyContext): Unifiable = context.get(this) match {
    case Some(next: Hole) => next.walk(context)
    case Some(next) => next
    case None => this
  }

  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = throw new IllegalStateException()
}

implicit class Tuple2Unifiable[T <: Unifiable, U <: Unifiable](tuple: Tuple2[T, U]) extends Unifiable {
  val get = tuple

  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = other match {
    case other: Tuple2Unifiable[Unifiable, Unifiable] => tuple._1.unify(context, other.get._1, tuple._2, other.get._2)
    case _ => UnifyResultFailure
  }
}

trait Constraint {
  val t: ConstraintT

  //val r: ConstraintT
  def reduce(context: Context): ReduceResult

  def deepReduce(context: Context): ReduceResult

  def reverse: Constraint
}

trait ConstraintOf[T <: ConstraintT] extends Constraint {
  override val t: T
  //override val r: R
  //import ConstraintOf.this.r.ev.a
  //import ConstraintOf.this.r.ev.b
  //override def reverse(context:Context): ConstraintOf.this.r.AConstraint
}

sealed trait ReduceResult

case object ReduceResultFailed extends ReduceResult

case object ReduceResultTrue extends ReduceResult

final case class ReduceResultOk(xs: Iterable[Constraint]) extends ReduceResult

trait ConstraintT {
  type ReverseT
  val reverse: ReverseT
  type AConstraint
  type AConstraintsInContext = Set[AConstraint]

  def incl(ctx: AConstraintsInContext, x: AConstraint): AConstraintsInContext = ctx.incl(x)

  def reduce(ctx: AConstraintsInContext): ConstraintTReduceResult

  protected final class Ev(implicit a: AConstraint <:< ConstraintOf[this.type], b: ReverseT <:< ConstraintT) // c: reverse.ReverseT =:= this.type

  val ev: Ev

  import ConstraintT.this.ev.a

  import ConstraintT.this.ev.b

}

sealed trait ConstraintTReduceResult // todo

// ctx: HashMap[(a: ConstraintT, a.AConstraintsInContext)] // todo
final case class Context(constraints: HashMap[ConstraintT, Any], goals: Iterable[Goal]) {
  def addConstraint(x: Constraint): Option[Context] = ???

  def addConstraints(xs: Iterable[Constraint]): Option[Context] = ???

  def addGoal(x: Goal): Option[Context] = ???

  def addGoals(xs: Iterable[Goal]): Option[Context] = xs.foldLeft(Some(this): Option[Context])((ctx, goal) => ctx.flatMap(_.addGoal(goal)))
}

final case class ContextNormalForm(constraints: HashMap[ConstraintT, Any])

type State = Iterable[Context]

implicit class StateImpl(x: State) {
  def addUnrolledGoal(goal: UnrolledGoal): State = (for {
    adds <- goal
    ctx <- x
  } yield ctx.addGoals(adds)).flatten

  def reduce: Option[State] = if (x.isEmpty) None else ???

  def run1: Option[(ContextNormalForm, State)] = ???

  def runAll: List[ContextNormalForm] = this.run1 match {
    case None => Nil
    case Some((x, s)) => x :: s.runAll
  }
}

// todo
type UnrolledGoal = Iterable[Iterable[Goal]]

object UnrolledGoal {
  val Succeed: UnrolledGoal = List(List())

  def andUnrolledGoal(x: UnrolledGoal, y: UnrolledGoal): UnrolledGoal = for {
    a <- x
    b <- y
  } yield a ++ b

  def andUnrolledGoals(xs: Iterable[UnrolledGoal]): UnrolledGoal = xs match {
    case Nil => Succeed
    case x :: xs => andUnrolledGoal(andUnrolledGoals(xs), x)
  }

  def orUnrolledGoal(x: UnrolledGoal, y: UnrolledGoal): UnrolledGoal = x ++ y

  def orUnrolledGoals(xs: Iterable[UnrolledGoal]): UnrolledGoal = xs.flatten

  def unrollUnrolled(x: UnrolledGoal): UnrolledGoal = orUnrolledGoals(x.map(universe => andUnrolledGoals(universe.map(_.unroll))))
}

sealed trait Goal {
  def reverse: Goal

  def unroll: UnrolledGoal
}

object Goal {
}

final case class GoalConstraint(x: Constraint) extends Goal {
  override def reverse: Goal = GoalConstraint(x.reverse)

  override def unroll: UnrolledGoal = List(List(this))
}

final case class GoalOr(x: Goal, y: Goal) extends Goal {
  override def reverse: Goal = GoalAnd(x.reverse, y.reverse)

  override def unroll: UnrolledGoal = List(List(x), List(y))
}

final case class GoalAnd(x: Goal, y: Goal) extends Goal {
  override def reverse: Goal = GoalOr(x.reverse, y.reverse)

  override def unroll: UnrolledGoal = List(List(x, y))
}

final case class GoalNot(x: Goal) extends Goal {
  lazy val get = x.reverse

  override def reverse: Goal = x

  override def unroll: UnrolledGoal = List(List(this.get))
}

final class GoalDelay(generate: => Goal) extends Goal {
  lazy val get = generate

  override def reverse: Goal = GoalDelay(this.get.reverse)

  override def unroll: UnrolledGoal = List(List(this.get))
}

object Goals {
}

/*
object Equal extends ConstraintT {
  type AConstraintsInContext = UnifyContext
}

object NotEqual extends ConstraintT {

}
*/
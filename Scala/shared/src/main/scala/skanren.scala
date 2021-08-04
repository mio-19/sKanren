package skanren

import scala.collection.immutable.HashMap
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LTT
import izumi.reflect.Tag

def typeOf[T](implicit ev: Tag[T]): LightTypeTag = ev.tag

final case class Unify(x: Unifiable, y: Unifiable) {
  def apply: UnifyResult = x.unify(y)
}

trait Unifiable {
  def unify(other: Unifiable): UnifyResult
}

sealed trait UnifyResult

case object UnifyResultFailed extends UnifyResult

case object UnifyResultTrue extends UnifyResult

final case class UnifyResultOk(xs: Iterable[Unify]) extends UnifyResult

trait Constraint {
  def reduce(context: Context): ReduceResult

  def deepReduce(context: Context): ReduceResult
}

sealed trait ReduceResult

case object ReduceResultFailed extends ReduceResult

case object ReduceResultTrue extends ReduceResult

final case class ReduceResultOk(xs: Iterable[Constraint]) extends ReduceResult

trait ConstraintT {
  type AConstraint
  implicit val ev: AConstraint <:< Constraint
  type AConstraintsInContext = Set[AConstraint]

  def incl(ctx: AConstraintsInContext, x: AConstraint): AConstraintsInContext = ctx.incl(x)

  def reduce(ctx: AConstraintsInContext): ConstraintTReduceResult
}

sealed trait ConstraintTReduceResult // todo

case class Context(ctx: HashMap[LightTypeTag, ConstraintT]) // todo

sealed trait Goal // todo

final case class GoalConstraint(x: Constraint) extends Goal

final case class GoalOr(x: Goal, y: Goal) extends Goal

final case class GoalAnd(x: Goal, y: Goal) extends Goal

final case class GoalNot(x: Goal) extends Goal

final class GoalDelay(x: => Goal) extends Goal {
  lazy val get = x
}

object Goals {
}


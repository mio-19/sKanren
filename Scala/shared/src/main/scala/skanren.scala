package skanren

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
}

sealed trait ReduceResult

case object ReduceResultFailed extends ReduceResult

case object ReduceResultTrue extends ReduceResult

final case class ReduceResultOk(xs: Iterable[Constraint]) extends ReduceResult

case class Context() // todo

object Goals {
  sealed trait Goal // todo
}


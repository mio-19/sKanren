package skanren

sealed trait Value

final case class Hole(identifier: Symbol) extends Value

final case class Atom(x: Symbol) extends Value

case object EmptyList extends Value

final case class NonEmptyList(x: Value, y: Value) extends Value

final case class Tagged(x: Value, y: Value) extends Value

sealed trait Relation extends Value {
  def apply(xs: List[Value]): Goal = ???
}

final case class InterpretedRelation(context: Context, args: List[Var], body: ExpGoal) extends Relation

final case class HostRelation(x: List[Value] => Goal) extends Relation

sealed trait Goal extends Value

final case class GoalConde(clauses: List[Goal]) extends Goal

sealed trait GoalConstraint extends Goal

final case class GoalEqual(x: Value, y: Value) extends GoalConstraint

final case class GoalNotEqual(x: Value, y: Value) extends GoalConstraint

final case class GoalAtom(x: Value) extends GoalConstraint

final case class GoalNotAtom(x: Value) extends GoalConstraint

sealed trait GoalDelay {
  def get: Goal = ???
}

final case class GoalApply(x: Value, xs: List[Value]) extends GoalDelay

final case class GoalInterpretedExists(x: Var, body: ExpGoal) extends GoalDelay

sealed trait ExpGoal

sealed trait ExpValue

final case class Quote(x: Value) extends ExpValue

case object QuoteNonEmptyList extends ExpValue

final case class ConsNonEmptyList(x: ExpValue, y: ExpValue) extends ExpValue

final case class Var(x: Atom) extends ExpValue

final case class Apply(x: ExpValue, xs: List[ExpValue]) extends ExpGoal

final case class Conde(clauses: List[List[ExpGoal]]) extends ExpGoal

final case class Equal(x: Exp, y: Exp) extends ExpGoal

final case class NotEqual(x: Exp, y: Exp) extends ExpGoal

final case class Atomo(x: Exp) extends ExpGoal

final case class NotAtomo(x: Exp) extends ExpGoal

final case class Lambda(x: Var, body: ExpGoal) extends ExpValue

final case class Letrec(xs: List[(Var, ExpValue)], body: ExpGoal) extends ExpGoal

final case class Exists(x: Var, body: ExpGoal) extends ExpGoal

// todo
final case class Context()

final case class Definitions(xs: List[(Var, ExpValue)])

// todo
final case class Universe()

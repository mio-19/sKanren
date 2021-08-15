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

final case class InterpretedRelation(context: Context, args: List[Var], body: Exp) extends Relation

final case class HostRelation(x: List[Value] => Goal) extends Relation

sealed trait Goal extends Value

final case class GoalConde(clauses: List[Goal]) extends Goal

final case class GoalEqual(x: Value, y: Value) extends Goal

final case class GoalNotEqual(x: Value, y: Value) extends Goal

sealed trait GoalDelay {
  def get: Goal = ???
}

final case class GoalApply(x: Value, xs: List[Value]) extends GoalDelay

sealed trait Exp

final case class Var(x: Atom) extends Exp

final case class Apply(x: Exp, xs: List[Exp]) extends Exp

final case class Conde(clauses: List[List[Exp]]) extends Exp

final case class Equal(x: Exp, y: Exp) extends Exp

final case class NotEqual(x: Exp, y: Exp) extends Exp

final case class Lambda(x: Var, body: Exp) extends Exp

final case class Letrec(xs: List[(Var, Exp)], body: Exp) extends Exp

// todo
final case class Context()

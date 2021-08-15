package skanren

// An embedded scheme and cKanren

// ??????????????????????????????????????????

sealed trait Value

final case class Hole(identifier: Symbol) extends Value

final case class Atom(x:Symbol) extends Value

case object EmptyList extends Value

final case class NonEmptyList(x:Value, y:Value) extends Value

final case class Tagged(x:Value,y:Value) extends Value

final case class Closure(context: Context, args: List[Var], body: Value) extends Value

// todo
sealed trait Goal extends Value

sealed trait Exp extends Value

final case class Var(x:Symbol) extends Exp

final case class Context()

package skanren

import scala.collection.immutable.{HashMap, HashSet}
import cats.implicits._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.{ParVector,ParSeq}

sealed trait Goal {
  def unroll: GoalConde = GoalConde(Vector(Vector(this)))
}
object Goal {
  def apply(generate: =>Goal):Goal = GoalDelay(generate)
}
final case class GoalConde(clauses: Vector[Vector[Goal]]) extends Goal {
  override def unroll: GoalConde = GoalConde.or(clauses.map(clause => GoalConde.and(clause.map(_.unroll))))
  override def toString: String = s"(conde ${clauses.map(_.mkString("(", " ", ")")).mkString(" ")})"
}
object GoalConde {
  def success:GoalConde = GoalConde(Vector(Vector()))
  def failure:GoalConde = GoalConde(Vector())
  def or(xs: Vector[GoalConde]):GoalConde=GoalConde(xs.map(_.clauses).flatten)
  def and(xs:GoalConde,ys:GoalConde):GoalConde = GoalConde(for {
    x <- xs.clauses
    y <- ys.clauses
  } yield x ++ y)
  def and(xs:Vector[GoalConde]):GoalConde=if(xs.isEmpty)success else xs.reduce(GoalConde.and(_,_))
}
final case class GoalNot(goal: Goal) extends Goal {
  override def toString: String = s"(not $goal)"
}
final class GoalDelay(generate: =>Goal) extends Goal {
  lazy val get:Goal = generate
  override def unroll: GoalConde = GoalConde(Vector(Vector(this.get)))
}
final case class GoalConstraint(x:Constraint[_]) extends Goal {
  override def toString: String = x.toString
}

// todo
final case class Context() {
  def getConstraintKind(kind: ConstraintKind): kind.ContextT = ???
  def updatedConstraintKind(kind: ConstraintKind, ctx: kind.ContextT): Context = ???
}

trait Constraint[Kind <: ConstraintKind] {
  val kind: Kind
}

trait ConstraintKind {
  type ContextT
  val empty: ContextT
  type ConstraintT
  type AdditionalConstraintT
  def adds(context: Context, xs: Vector[ConstraintT]): Option[(ConstraintT, AdditionalConstraintT)]
}
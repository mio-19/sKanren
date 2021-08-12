package skanren

import scala.collection.immutable.{HashMap, HashSet}
import cats.implicits._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.{ParVector, ParSeq}

sealed trait Goal {
  def unroll: GoalConde = GoalConde(Vector(Vector(this)))
}

object Goal {
  def apply(generate: => Goal): Goal = GoalDelay(generate)
}

final case class GoalConde(clauses: Vector[Vector[Goal]]) extends Goal {
  override def unroll: GoalConde = GoalConde.or(clauses.map(clause => GoalConde.and(clause.map(_.unroll))))

  override def toString: String = s"(conde ${clauses.map(_.mkString("(", " ", ")")).mkString(" ")})"
}

object GoalConde {
  def success: GoalConde = GoalConde(Vector(Vector()))

  def failure: GoalConde = GoalConde(Vector())

  def or(xs: Vector[GoalConde]): GoalConde = GoalConde(xs.map(_.clauses).flatten)

  def and(xs: GoalConde, ys: GoalConde): GoalConde = GoalConde(for {
    x <- xs.clauses
    y <- ys.clauses
  } yield x ++ y)

  def and(xs: Vector[GoalConde]): GoalConde = if (xs.isEmpty) success else xs.reduce(GoalConde.and(_, _))
}

final case class GoalNot(goal: Goal) extends Goal {
  override def toString: String = s"(not $goal)"
}

final class GoalDelay(generate: => Goal) extends Goal {
  lazy val get: Goal = generate

  override def unroll: GoalConde = GoalConde(Vector(Vector(this.get)))
}

final case class GoalConstraint(x: Constraint[_]) extends Goal {
  override def toString: String = x.toString
}

type Context = HashMap[ConstraintKind, Any]

implicit class ContextOps(map: Context) {
  def getByKind(kind: ConstraintKind): kind.ContextT = map.getOrElse(kind, kind.empty).asInstanceOf[kind.ContextT]

  def updatedByKind(kind: ConstraintKind, ctx: kind.ContextT): Context = map.updated(kind, ctx)

  def mapByKind(kind: ConstraintKind, f: kind.ContextT => kind.ContextT): Context = this.updatedByKind(kind, f(this.getByKind(kind)))

  def step: Option[Context] = map.keys.map(kind => kind.step(map).map(ctx => (kind, ctx))).toVector.sequence.map(HashMap.empty.concat(_))

  protected def addConstraintsByKind(kind: ConstraintKind, xs: Vector[Constraint[_]]): Option[Context] = kind.addConstraints(map, xs.asInstanceOf[Vector[kind.ConstraintT]]).map(r => this.updatedByKind(kind, r._1))

  def addConstraints(xs: Vector[Constraint[_]]): Option[Context] = xs.groupBy(x => x.kind: ConstraintKind).foldLeft(Some(map): Option[Context])((ctx, kindXs) => ctx.flatMap(_.addConstraintsByKind(kindXs._1, kindXs._2)))
}

type NegContext = HashMap[ConstraintKind, Vector[Any]]

implicit class NegContextOps(map: NegContext) {
  def getByKind(kind: ConstraintKind): kind.AdditionalsT = map.getOrElse(kind, kind.emptyAdditionals).asInstanceOf[kind.AdditionalsT]

  def updatedByKind(kind: ConstraintKind, x: kind.AdditionalsT): NegContext = map.updated(kind, x)

  def mapByKind(kind: ConstraintKind, f: kind.AdditionalsT => kind.AdditionalsT): NegContext = this.updatedByKind(kind, f(this.getByKind(kind)))
}

final case class Universe(context: Context, goals: Vector[Goal], negState: NegState) {
}

// Every NegUniverse must not hold.
type NegState = Vector[NegUniverse]

final case class NegUniverse(negContext: NegContext, negGoals: Vector[Goal])

type State = Vector[Universe]

trait Constraint[Kind <: ConstraintKind] {
  val kind: Kind
}

trait ConstraintKind {
  type ContextT
  val empty: ContextT
  type ConstraintT
  type AdditionalConstraintT
  final type AdditionalsT = Vector[AdditionalConstraintT]
  final val emptyAdditionals: AdditionalsT = Vector()

  def addConstraints(context: Context, xs: Vector[ConstraintT]): Option[(ContextT, AdditionalsT)]

  def addAdditionals(context: Context, xs: AdditionalsT): Option[(ContextT, AdditionalsT)]

  // called when other Constraints are changed
  def step(context: Context): Option[ContextT] = Some(context.getByKind(this))

  protected final class Ev(implicit a: ConstraintT <:< Constraint[this.type])

  protected val ev: Ev

  import ConstraintKind.this.ev.a

}

type UnificationContext = HashMap[Hole, Unifiable]

object UnificationContext {
  val empty: UnificationContext = HashMap()
}

implicit class UnificationContextOps(context: UnificationContext) {
  def walkOption(x: Hole): Option[Unifiable] = context.get(x)

  def walkOption(x: Unifiable): Option[Unifiable] = x match {
    case x: Hole => this.walkOption(x)
    case _ => None
  }
}

object Unification extends ConstraintKind {
  override type ContextT = UnificationContext
  override val empty = UnificationContext.empty
  override type AdditionalConstraintT = NormalUnify
  override type ConstraintT = Unify

  override def addConstraints(context: Context, xs: Vector[ConstraintT]): Option[(ContextT, Vector[AdditionalConstraintT])] = ???

  override def addAdditionals(context: Context, xs: Vector[AdditionalConstraintT]): Option[(ContextT, Vector[AdditionalConstraintT])] = ???

  override protected val ev: Ev = Ev()
}

final case class Unify(x: Unifiable, y: Unifiable) extends Constraint[Unification.type] {
  override val kind = Unification
}

final case class NormalUnify(hole: Hole, value: Unifiable)

type UnifyResult = Option[(UnificationContext, Vector[NormalUnify])]

object UnifyResult {
  def success(x: UnificationContext) = Some(x, Vector())

  def success(x: UnificationContext, normal: NormalUnify) = Some(x.updated(normal.hole, normal.value), Vector(normal))

  def success(x: UnificationContext, hole: Hole, value: Unifiable) = Some(x.updated(hole, value), Vector(NormalUnify(hole, value)))

  def failure: UnifyResult = None
}

trait Unifiable {
  def impl_unify(context: UnificationContext, other: Any): UnifyResult = if (this == other) UnifyResult.success(context) else UnifyResult.failure

  final def unify(context: UnificationContext, other: Unifiable): UnifyResult = if (this == other) UnifyResult.success(context) else context.walkOption(this) match {
    case Some(self) => self.unify(context, other)
    case None => context.walkOption(other) match {
      case Some(other) => this.unify(context, other)
      case None => other match {
        case wrapper: Wrapper => {
          val unboxed = wrapper.unbox
          if (this == unboxed) UnifyResult.success(context) else unboxed match {
            case unboxed: Unifiable => this.unify(context, unboxed)
            case unboxed => this match {
              case self: Hole => UnifyResult.success(context, self, other)
              case _ => this.impl_unify(context, unboxed)
            }
          }
        }
        case other => this match {
          case self: Hole => UnifyResult.success(context, self, other)
          case _ => this.impl_unify(context, other)
        }
      }
    }
  }
}

trait Wrapper {
  def impl_unbox: Any

  final def unbox: Any = Wrapper.unbox(this.impl_unbox)
}

object Wrapper {
  def unbox(x: Any): Any = x match {
    case x: Wrapper => x.unbox
    case x => x
  }
}

final case class Hole(identifier: Symbol) extends Unifiable {
  override def impl_unify(context: UnificationContext, other: Any): UnifyResult = throw new UnsupportedOperationException()
}
package skanren

import scala.collection.immutable.HashMap
import izumi.reflect.macrortti.LightTypeTag
import izumi.reflect.macrortti.LTT
import izumi.reflect.Tag

def typeOf[T](implicit ev: Tag[T]): LightTypeTag = ev.tag

final case class UnifyNormalForm(x: Hole, y: Unifiable)

type UnifyContext = HashMap[Hole, Unifiable]

object UnifyContext {
  val Default: UnifyContext = HashMap()
}

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
      case (None, None) => Some(context, List(UnifyNormalForm(self, other)))
    }
    case (self: Hole, other) => self.walkOption(context) match {
      case Some(self) => other.unify(context, self)
      case None => Some(context, List(UnifyNormalForm(self, other)))
    }
    case (self, other: Hole) => other.unify(context, self)
    case (self, other) => self.impl_unify(context, other)
  }

  final def unify(_context: UnifyContext, other: Unifiable, normal: UnifyResult): UnifyResult = for {
    (ctx1, xs) <- normal
    (ctx2, ys) <- this.unify(ctx1, other)
  } yield (ctx2, ys++xs)

  final def unify(context: UnifyContext, other: Unifiable, x: Unifiable, y: Unifiable): UnifyResult = this.unify(context, other, x.unify(context, y))

  final def unify(other: Unifiable): UnifyResult = this.unify(UnifyContext.Default, other)
}

trait UnifiableAtom extends Unifiable {
  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = if (this == other) Some((context,Nil)) else None
}

trait Unifitor[T] {
  def impl_unify(self: T, context: UnifyContext, other: Any): UnifyResult

  final def unify(self: T, context: UnifyContext, other: Any): UnifyResult = (self, other) match {
    case (self, other: UnifitorWrapper[_]) => this.unify(self, context, other.get)
    case (self, other: Hole) => other.unify(context, UnifitorWrapper(self)(this))
    case (self, other) => this.impl_unify(self, context, other)
  }

  final def unify(self: T, _context: UnifyContext, other: Any, normal: UnifyResult): UnifyResult = for {
    (ctx1, xs) <- normal
    (ctx2, ys) <- this.unify(self, ctx1, other)
  } yield (ctx2, ys ++ xs)

  final def unify[U](self: T, context: UnifyContext, other: Any, x: U, y: Any)(implicit u: Unifitor[U]): UnifyResult = this.unify(self, context, other, u.unify(x, context, y))
}

trait UnifitorAtom[T] extends Unifitor[T] {
  def impl_unify(self: T, context: UnifyContext, other: Any): UnifyResult = if (self == other) Some((context, Nil)) else None
}

trait AbstractUnifiableWrapper {

}

implicit class UnifitorWrapper[T](x: T)(implicit instance: Unifitor[T]) extends Unifiable {
  if (x.isInstanceOf[UnifitorWrapper[_]]) {
    throw new IllegalArgumentException()
  }
  private[skanren] val get = x
  private val getInstance = instance

  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = instance.unify(x, context, other)
}

implicit object SymbolUnifitor extends UnifitorAtom[Symbol]

/*
implicit class Tuple2Unifiable[T <: Unifiable, U <: Unifiable](tuple: Tuple2[T, U]) extends Unifiable {
  val get = tuple

  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = other match {
    case other: Tuple2Unifiable[Unifiable, Unifiable] => tuple._1.unify(context, other.get._1, tuple._2, other.get._2)
    case _ => UnifyResultFailure
  }
}
*/
final case class Tuple2Unifitor[T, U]()(implicit t: Unifitor[T], u: Unifitor[U]) extends Unifitor[Tuple2[T, U]] {
  override def impl_unify(self: Tuple2[T, U], context: UnifyContext, other: Any): UnifyResult = other match {
    case other: Tuple2[_, _] => t.unify(self._1, context, other._1, self._2, other._2)
  }
}

implicit class Tuple2Unifiable[T, U](x: Tuple2[T, U])(implicit t: Unifitor[T], u: Unifitor[U]) extends UnifitorWrapper(x)(Tuple2Unifitor()(t, u))

final class UnifiableUnifitor[T <: Unifiable] extends Unifitor[T] {
  override def impl_unify(self: T, context: UnifyContext, other: Any): UnifyResult = other match {
    case other: Unifiable => self.unify(context, other)
    case _ => UnifyResultFailure
  }
}

implicit val unifiableUnifitor: Unifitor[Unifiable] = UnifiableUnifitor[Unifiable]
implicit val holeUnifitor: Unifitor[Hole] = UnifiableUnifitor[Hole]

type UnifyResult = Option[(UnifyContext, List[UnifyNormalForm])]

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

trait Constraint {
  val t: ConstraintT

  //val r: ConstraintT

  def reverse: Constraint
}

trait ConstraintOf[T <: ConstraintT] extends Constraint {
  override val t: T
  //override val r: R
  //import ConstraintOf.this.r.ev.a
  //import ConstraintOf.this.r.ev.b
  //override def reverse(context:Context): ConstraintOf.this.r.AConstraint
}

type ReduceResult = Option[Iterable[Constraint]]

trait ConstraintT {
  final def getFromOption(ctx: Context): Option[AConstraintsInContext] = ctx.constraints.get(this).asInstanceOf[Option[AConstraintsInContext]]

  type ReverseT
  val reverse: ReverseT
  type AConstraint
  type AConstraintsInContext
  val default: AConstraintsInContext

  def incl(ctx: AConstraintsInContext, x: AConstraint): Option[AConstraintsInContext]

  def incls(ctx: AConstraintsInContext, xs: List[AConstraint]): Option[AConstraintsInContext] = xs match {
    case Nil => Some(ctx)
    case x :: Nil => this.incl(ctx, x)
    case x :: xs => this.incl(ctx, x).flatMap(this.incls(_, xs))
  }

  def normalForm(ctx: AConstraintsInContext): Option[AConstraintsInContext] = Some(ctx)

  protected final class Ev(implicit a: AConstraint <:< ConstraintOf[this.type], b: ReverseT <:< ConstraintT) // c: reverse.ReverseT =:= this.type

  val ev: Ev

  import ConstraintT.this.ev.a

  import ConstraintT.this.ev.b

}

trait ConstraintTSet extends ConstraintT {
  override type AConstraintsInContext = Set[AConstraint]

  override def incl(ctx: AConstraintsInContext, x: AConstraint): Option[AConstraintsInContext] = Some(ctx.incl(x))
}

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

final case class Unify(x: Unifiable, y: Unifiable) extends ConstraintOf[Equal.type] {
  override val t = Equal

  override def reverse: NegativeUnify = NegativeUnify(x, y)

  def apply(context: UnifyContext): UnifyResult = x.unify(context, y)
}

object Equal extends ConstraintT {
  override type AConstraintsInContext = UnifyContext
  override type AConstraint = Unify
  override type ReverseT = NotEqual.type
  override val reverse = NotEqual
  override val default = UnifyContext.Default

  override def incl(ctx: AConstraintsInContext, x: AConstraint): Option[AConstraintsInContext] = x(ctx) match {
    case Some(newctx, _adds) => Some(newctx)
    case None => None
  }

  override val ev = Ev()
}

final case class NegativeUnify(x: Unifiable, y: Unifiable) extends ConstraintOf[NotEqual.type] {
  override val t = NotEqual

  override def reverse: Unify = Unify(x, y)
}

object NotEqual extends ConstraintT {
  override type AConstraintsInContext = UnifyContext
  override type AConstraint = NegativeUnify
  override type ReverseT = Equal.type
  override val reverse = Equal
  override val default = UnifyContext.Default

  override def incl(ctx: AConstraintsInContext, x: AConstraint): Option[AConstraintsInContext] = ???

  override def normalForm(ctx: AConstraintsInContext): Option[AConstraintsInContext] = ???

  override val ev = Ev()
}

trait Generator[T] {
  val generate: LazyList[T]
}

def mergeLazyList[T, U](xs: LazyList[T], ys: LazyList[U]): LazyList[T | U] = xs match {
  case head #:: tail => head #:: mergeLazyList(ys, tail)
  case _ => ys
}

final case class SimpleGenerator[T](override val generate: LazyList[T]) extends Generator[T]

trait FiniteGenerator[T] extends Generator[T] {
  override val generate: LazyList[T] = LazyList.empty.concat(generateFinite)
  lazy val generateFinite: List[T]
}

implicit class GeneratorImpl[T](x: Generator[T]) {
  def or[U](y: Generator[U]): Generator[T | U] = SimpleGenerator(mergeLazyList(x.generate, y.generate))
}

object generators {
}
package skanren

import scala.collection.immutable.{HashMap, HashSet}
import cats.implicits._
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParVector
import scala.collection.parallel.immutable.ParSeq

//import izumi.reflect.macrortti.LightTypeTag
//import izumi.reflect.macrortti.LTT
//import izumi.reflect.Tag
//def typeOf[T](implicit ev: Tag[T]): LightTypeTag = ev.tag

implicit class mapSequenceParVector[T](xs:ParVector[T]) {
  // todo: optimize me
  def mapSequence[U](x:T=>Option[U]):Option[ParVector[U]] = xs.map(x).seq.sequence.map(_.par)
}

final case class UnifyNormalForm(x: Hole, y: Unifiable) {
  override def toString:String = s"(==n $x $y)"
}

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

  final def unify(context: UnifyContext, other: Unifiable): UnifyResult = if(this==other) Some((context, Nil)) else (this, other) match {
    case (self:UnifiableWrapper,other)=>self.unbox.unify(context,other)
    case (self,other:UnifiableWrapper)=>self.unify(context,other.unbox)
    case (self: Hole, other: Hole) => (self.walkOption(context), other.walkOption(context)) match {
      case (Some(self), Some(other)) => self.unify(context, other)
      case (Some(self), None) => self.unify(context, other)
      case (None, Some(other)) => self.unify(context, other)
      case (None, None) => {
        val result = UnifyNormalForm(self, other)
        Some(context.add(result), List(result))
      }
    }
    case (self: Hole, other) => self.walkOption(context) match {
      case Some(self) => other.unify(context, self)
      case None => {
        val result = UnifyNormalForm(self, other)
        Some(context.add(result), List(result))
      }
    }
    case (self, other: Hole) => other.unify(context, self)
    case (self, other) => self.impl_unify(context, other)
  }

  final def unify(_context: UnifyContext, other: Unifiable, normal: UnifyResult): UnifyResult = for {
    (ctx1, xs) <- normal
    (ctx2, ys) <- this.unify(ctx1, other)
  } yield (ctx2, ys ++ xs)

  final def unify(context: UnifyContext, other: Unifiable, x: Unifiable, y: Unifiable): UnifyResult = this.unify(context, other, x.unify(context, y))

  final def unify(other: Unifiable): UnifyResult = this.unify(UnifyContext.Default, other)
}

trait Wrapper {
  def unbox:Any
}
trait UnifiableWrapper extends Wrapper{
  override def unbox:Unifiable
}

trait Readbackable {
  def readback(context: UnifyContext): Any
  final def readback(context:Context):Any = this.readback(Equal.getFromOrDefault(context))
  final def readback(context:ContextNormalForm):Any=this.readback(context.toContext)
}

trait UnifiableAtom extends Unifiable {
  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = if (this == other) Some((context, Nil)) else None
}

trait ReadbackableAtom extends Readbackable {
  override def readback(context: UnifyContext): Any = this
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

trait Readbacker[T] {
  def readback(self:T,context:UnifyContext):Any
}

trait UnifitorAtom[T] extends Unifitor[T] {
  def impl_unify(self: T, context: UnifyContext, other: Any): UnifyResult = if (self == other) Some((context, Nil)) else None
}

implicit class UnifitorWrapper[T](x: T)(implicit instance: Unifitor[T]) extends Unifiable {
  if (x.isInstanceOf[UnifitorWrapper[_]]) {
    throw new IllegalArgumentException()
  }
  private[skanren] val get = x
  private val getInstance = instance

  override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = instance.unify(x, context, other)
}

trait ReadbackerAtom[T] extends Readbacker[T] {
  def readback(self:T,_context:UnifyContext):Any = self
}

implicit class ReadbackerWrapper[T](x:T)(implicit instance: Readbacker[T]) extends Readbackable {
  override def readback(context: UnifyContext): Any = instance.readback(x,context)
}

implicit object SymbolUnifitor extends UnifitorAtom[Symbol]
implicit object SymbolReadbacker extends ReadbackerAtom[Symbol]
implicit object UnitUnifitor extends UnifitorAtom[Unit]
implicit object UnitReadbacker extends ReadbackerAtom[Unit]
implicit object BooleanUnifitor extends UnifitorAtom[Boolean]
implicit object BooleanReadbacker extends ReadbackerAtom[Boolean]

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

final case class Tuple2Readbacker[T,U]()(implicit t:Readbacker[T],u:Readbacker[U]) extends Readbacker[Tuple2[T,U]] {
  override def readback(self:Tuple2[T,U],context:UnifyContext):Any = (t.readback(self._1,context),u.readback(self._2,context))
}

implicit class Tuple2Unifiable[T, U](x: Tuple2[T, U])(implicit t: Unifitor[T], u: Unifitor[U]) extends UnifitorWrapper(x)(Tuple2Unifitor()(t, u))
implicit class Tuple2Readbackable[T,U](x:Tuple2[T,U])(implicit t:Readbacker[T],u:Readbacker[U]) extends ReadbackerWrapper(x)(Tuple2Readbacker()(t,u))

final class UnifiableUnifitor[T <: Unifiable] extends Unifitor[T] {
  override def impl_unify(self: T, context: UnifyContext, other: Any): UnifyResult = other match {
    case other: Unifiable => self.unify(context, other)
    case _ => UnifyResultFailure
  }
}
final class ReadbackableReadbacker[T<:Readbackable] extends Readbacker[T] {
  override def readback(self:T,context:UnifyContext):Any = self.readback(context)
}

implicit val unifiableUnifitor: Unifitor[Unifiable] = UnifiableUnifitor[Unifiable]

type UnifyResult = Option[(UnifyContext, List[UnifyNormalForm])]

val UnifyResultFailure = None

private object Counter {
  private var counter: Int = 0
   def gen: Int = this.synchronized {
      val c = counter
      counter = counter + 1
      c
    }

}

object Hole {
  def fresh[T](x: Hole => T): T = x(Hole(Symbol("#" + Counter.gen.toHexString))) // x.hashCode.toHexString

  def fresh[T](name: String, x: Hole => T): T = x(Hole(Symbol(name + "#" + Counter.gen.toHexString))) // x.hashCode.toHexString
}
implicit val holeUnifitor: Unifitor[Hole] = UnifiableUnifitor[Hole]
implicit val holeReadbacker: Readbacker[Hole] = ReadbackableReadbacker[Hole]
final case class Hole(identifier: Symbol) extends Unifiable with Readbackable {
  override def readback(context:UnifyContext):Any = context.getOrElse(this, this)
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
  override def toString:String="*"+identifier.name
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

  final def getFromOrDefault(ctx: Context): AConstraintsInContext = getFromOption(ctx).getOrElse(default)

  final def setIn(ctx:Context, x: AConstraintsInContext): Context = ctx match {
    case Context(constraints, goals) => Context(constraints.updated(this, x), goals)
  }

  type ReverseT
  val reverse: ReverseT
  type AConstraint
  type AConstraintsInContext
  val default: AConstraintsInContext

  def incl(ctx: Context, x: AConstraint): Option[AConstraintsInContext] = this.incls(ctx, List(x))

  def incls(ctx: Context, xs: List[AConstraint]): Option[AConstraintsInContext] = xs match {
    case Nil => Some(getFromOrDefault(ctx))
    case x :: Nil => this.incl(ctx, x)
    case x :: xs => this.incl(ctx, x).flatMap(a=>this.incls(setIn(ctx,a), xs))
  }

  def normalForm(ctx: Context): Option[AConstraintsInContext] = Some(getFromOrDefault(ctx))

  protected final class Ev(implicit a: AConstraint <:< ConstraintOf[this.type], b: ReverseT <:< ConstraintT) // c: reverse.ReverseT =:= this.type

  val ev: Ev

  import ConstraintT.this.ev.a

  import ConstraintT.this.ev.b

}

trait ConstraintTSet extends ConstraintT {
  override type AConstraintsInContext = Set[AConstraint]

  override def incl(ctx: Context, x: AConstraint): Option[AConstraintsInContext] = Some(getFromOrDefault(ctx).incl(x))
}

// ctx: HashMap[(a: ConstraintT, a.AConstraintsInContext)]
// todo: replace HashMap
// todo: record constraint types in type?
final case class Context(constraints: HashMap[ConstraintT, Any], goals: List[Goal]) {
  def addConstraint(x: Constraint): Option[Context] = for {
    newT <- x.t.incl(this,x.asInstanceOf[x.t.AConstraint])
  } yield x.t.setIn(this, newT)

  def addGoal(x: Goal): Option[Context] = addGoals(List(x))

  def addGoals(xs: List[Goal]): Option[Context] = {
    val (newConstraints0, newGoals) = xs.partition(_.isInstanceOf[GoalConstraint])
    val newConstraints = newConstraints0.map(_.asInstanceOf[GoalConstraint].x)
    val newcs = Context.listABToMapAListB(newConstraints.map(x=>(x.t, x)), HashMap()).toList
    Context.doConstraintss(Context(this.constraints,goals++newGoals),newcs)
  }.flatMap(_.stepConstraints)
  def stepConstraints: Option[Context] = for {
    newC <- constraints.keys.toList.map(t=>t.normalForm(this).map((t,_))).sequence
  } yield Context(HashMap.empty.concat(newC),goals)
  //def toNormalIfIs: Option[ContextNormalForm] = if (goals.isEmpty) Some(ContextNormalForm(constraints)) else None
  def caseNormal: Either[Context, ContextNormalForm] =if (goals.isEmpty) Right(ContextNormalForm(constraints)) else Left(this)
}
object Context {
  val Empty = Context(HashMap(),List())
private def listABToMapAListB[A,B](xs:List[(A,B)], base: HashMap[A,List[B]]):HashMap[A,List[B]] = xs match {
  case (k,v)::xs=> listABToMapAListB(xs, base.updated(k,v::base.getOrElse(k,Nil)))
  case Nil => base
}

  private def doConstraints(ctx:Context, t: ConstraintT, cs: List[Constraint]): Option[Context] = for {
    newT <- t.incls(ctx, cs.map(_.asInstanceOf[t.AConstraint]))
  } yield t.setIn(ctx,newT)
  private def doConstraintss(ctx:Context,xs:List[(ConstraintT,List[Constraint])]):Option[Context] = xs match {
    case (t,cs)::xs=>for {
      a <- doConstraints(ctx,t,cs)
      result <- doConstraintss(a, xs)
    } yield result
    case Nil => Some(ctx)
  }
}

final case class ContextNormalForm(constraints: HashMap[ConstraintT, Any]) {
  def toContext:Context=Context(constraints,List())
}

type State = ParSeq[Context]
object State {
  val Empty = ParSeq(Context.Empty)
}

implicit class StateImpl(x: State) {
  def addConde(goal: GoalConde): State = (for {
    adds <- goal.clauses.par
    ctx <- x
  } yield ctx.addGoals(adds)).flatten
  def addGoal(goal: Goal): State = addConde(Goal.unrollN(goal))

  def step: Option[State] = if (x.isEmpty) None else Some(x.flatMap({ case ctx@Context(constraints, goals0) =>
    if (goals0.isEmpty) Seq(ctx) else {
      val ctx0 = Context(constraints, List())
      (for {
        goals <- Goal.unrollN(goals0).clauses.par
      } yield ctx0.addGoals(goals)).flatten
    }}))

  def run1: Option[(Seq[ContextNormalForm], State)] = this.step.flatMap({xs =>
    xs.map(_.caseNormal).partition(_.isRight) match {
      case (rights,lefts) =>{
        val ctxs = lefts.map(_.left.get)
        val normals = rights.map(_.right.get)
        if (normals.isEmpty) ctxs.run1 else Some(normals.seq, ctxs)
      }
    }
  })

  def runAll: List[ContextNormalForm] = this.run1 match {
    case None => Nil
    case Some((x, s)) => x.toList ++ s.runAll
  }
}

sealed trait Goal {
  def reverse: Goal

  def unroll: GoalConde = GoalConde(this)

  final def runAll: List[ContextNormalForm] = State.Empty.addGoal(this).runAll
  final def run1= State.Empty.addGoal(this).run1

  final def &&(other:Goal):Goal=Goal.and(List(this,other))
  final def ||(other:Goal):Goal=Goal.or(List(this,other))
  final def unary_! :Goal=GoalNot(this)
}

object Goal {
  def apply(x: =>Goal):Goal = GoalDelay({x})
  def exists(x: Hole=>Goal): Goal = Hole.fresh(x)
  def exists(name:String,x: Hole=>Goal):Goal=Hole.fresh(name,x)
  def implies(a:Goal, b:Goal): Goal = (!a)||b
  def forall(x:Hole=>(Goal, Goal)):Goal = Goal.exists( hole => {
    val g = x(hole)
    Goal.implies(g._1,g._2)
  })
  def forall(name:String,x:Hole=>(Goal, Goal)):Goal = Goal.exists(name, hole => {
    val g = x(hole)
    Goal.implies(g._1,g._2)
  })
  def or(xs:List[Goal]):Goal=GoalConde(xs.map(List(_)))
  def or(xs:Seq[Goal]):Goal=or(xs.toList)
  def and(xs:List[Goal]):Goal=GoalConde(List(xs))
  def and(xs:Seq[Goal]):Goal=and(xs.toList)
  val Success = GoalConde.Success
  val Failure = GoalConde.Failure
  def unrollN(x:Goal):GoalConde = x.unroll.unroll.unroll.unroll.unroll.unroll.unroll.unroll
  def unrollN(xs:ParSeq[Goal]):GoalConde = if (xs.isEmpty) throw new IllegalArgumentException() else xs.map(Goal.unrollN(_)).reduce(_&&_)
  def unrollN(xs:List[Goal]):GoalConde = unrollN(xs.par)
}

final case class GoalConstraint(x: Constraint) extends Goal {
  override def reverse: Goal = GoalConstraint(x.reverse)
  override def toString:String = x.toString
}

final case class GoalConde(clauses: List[List[Goal]]) extends Goal {
  override def reverse: Goal = Goal.and(clauses.map(clause=>Goal.or(clause.map(_.reverse))))
   def &&(other:GoalConde):GoalConde = GoalConde(for {
    self <- this.clauses
    o <- other.clauses
  } yield self ++ o)
   def ||(other:GoalConde):GoalConde = GoalConde(this.clauses++other.clauses)
  override def unroll: GoalConde = GoalConde.or(clauses.map(clause=>GoalConde.and(clause.map(_.unroll))))
  override def toString:String=s"(conde ${clauses.map(_.mkString("("," ",")")).mkString(" ")})"
}
object GoalConde {
  def apply(x:Goal):GoalConde = apply(List(List(x)))
  def apply(clauses: List[List[Goal]]):GoalConde=new GoalConde(clauses)
    def apply(clauses: Seq[List[Goal]]):GoalConde=apply(clauses.toList)
val Success :GoalConde= GoalConde(List(List()))
val Failure:GoalConde=GoalConde(List())
  def or(xs:List[GoalConde]):GoalConde = xs match {
    case x :: Nil => x
    case x :: xs => x || GoalConde.or(xs)
    case Nil => Goal.Failure
  }
  def and(xs:List[GoalConde]):GoalConde = xs match {
    case x::Nil=>x
    case x::xs => x && GoalConde.and(xs)
    case Nil => Goal.Success
  }
}

final case class GoalNot(x: Goal) extends Goal {
  lazy val get = x.reverse

  override def reverse: Goal = x

  override def unroll: GoalConde = this.get.unroll
  override def toString:String=s"(not $x)"
}

final class GoalDelay(generate: => Goal) extends Goal {
  lazy val get = generate

  override def reverse: Goal = GoalDelay(this.get.reverse)

  override def unroll: GoalConde = GoalConde(this.get)
}

final case class Unify(x: Unifiable, y: Unifiable) extends ConstraintOf[Equal.type] {
  override val t = Equal

  override def reverse: NegativeUnify = NegativeUnify(x, y)

  def apply(context: UnifyContext): UnifyResult = x.unify(context, y)
  override def toString:String=s"(=== $x $y)"
}

object Equal extends ConstraintT {
  override type AConstraintsInContext = UnifyContext
  override type AConstraint = Unify
  override type ReverseT = NotEqual.type
  override val reverse = NotEqual
  override val default = UnifyContext.Default

  override def incl(ctx: Context, x: AConstraint): Option[AConstraintsInContext] = x(getFromOrDefault(ctx)) match {
    case Some(newctx, _adds) => Some(newctx)
    case None => None
  }

  override val ev = Ev()
}

final case class NegativeUnify(x: Unifiable, y: Unifiable) extends ConstraintOf[NotEqual.type] {
  override val t = NotEqual

  override def reverse: Unify = Unify(x, y)
  override def toString:String=s"(=/= $x $y)"
}

object NotEqual extends ConstraintT {
  override type AConstraintsInContext = ParVector[UnifyNormalForm]
  override type AConstraint = NegativeUnify
  override type ReverseT = Equal.type
  override val reverse = Equal
  override val default = ParVector()

  private def toNegativeUnify(x: UnifyNormalForm): NegativeUnify = x match {
    case UnifyNormalForm(a, b) => NegativeUnify(a, b)
  }

  override def incls(ctx: Context, xs: List[AConstraint]): Option[AConstraintsInContext] = for {
    adds <- norms(Equal.getFromOrDefault(ctx), xs)
  } yield getFromOrDefault(ctx)++adds

  private def norms(equalCtx: UnifyContext, xs: List[NegativeUnify]): Option[List[UnifyNormalForm]] = xs.map(norm(equalCtx, _)).sequence.map(_.flatten)
  private def norm(equalCtx: UnifyContext, x: NegativeUnify): Option[List[UnifyNormalForm]] = x match {
    case NegativeUnify(a, b) => a.unify(equalCtx, b) match {
      case None => Some(List())
      case Some(ctx, Nil) => None
      case Some(ctx, adds@(_ :: _)) => Some(adds)
    }
  }

  private def normal(equalCtx: UnifyContext, notEqCtx: AConstraintsInContext): Option[AConstraintsInContext] =notEqCtx.mapSequence(x=>norm(equalCtx,toNegativeUnify(x))).map(_.flatten)
  override def normalForm(ctx: Context): Option[AConstraintsInContext] = normal(Equal.getFromOrDefault(ctx), getFromOrDefault(ctx))

  override val ev = Ev()
}

// todo
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


import scala.annotation.targetName

def exists(x: Hole=>Goal) = Goal(Goal.exists(x))
def exists(name:String,x: Hole=>Goal) = Goal(Goal.exists(name,x))
def begin(xs: =>Goal*): Goal = Goal.and(xs)
def conde(xs: =>List[Goal]*):Goal = GoalConde(xs)
@targetName("condeOr") def conde(xs: =>Goal*):Goal = Goal.or(xs)
implicit class UnifiableOps[T](x:T)(implicit ev: T <:< Unifiable) {
  def ===[U<:Unifiable](other:U) = GoalConstraint(Unify(x,other))
  def =:=[U<:Unifiable](other:U) = GoalConstraint(Unify(x,other))
  def =/=[U<:Unifiable](other:U) = GoalConstraint(NegativeUnify(x,other))
}
/*
implicit class UnifitorOps[T](x:T)(implicit ev: Unifitor[T]) {
  def ===[U<:Unifiable](other:U) = GoalConstraint(Unify(x,other))
  def =:=[U<:Unifiable](other:U) = GoalConstraint(Unify(x,other))
  def =/=[U<:Unifiable](other:U) = GoalConstraint(NegativeUnify(x,other))
}
*/
object sexp {
  import scala.language.implicitConversions
implicit val SExpUnifitor: Unifitor[SExp] = UnifiableUnifitor[SExp]
implicit val SExpReadbacker: Readbacker[SExp] = ReadbackableReadbacker[SExp]
  sealed trait SExp extends Unifiable with Readbackable {
    def quasiStrList: String = s" . ${this.quasiquoteString})"
    def quasiquoteString:String = this.toString
  }
  case object Empty extends SExp with UnifiableAtom with ReadbackableAtom {
    override def quasiStrList: String = ")"
    override def toString:String = "()"
  }
  final case class Pair(x:SExp,y:SExp) extends SExp with Unifiable with Readbackable {
    override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = other match {
      case Pair(x1,y1)=>x.unify(context,x1,y,y1)
      case _ => UnifyResultFailure
    }
    override def readback(context:UnifyContext):Any = Pair(x.readback(context).asInstanceOf[SExp],y.readback(context).asInstanceOf[SExp])
    override def quasiStrList: String = s" ${x.quasiquoteString}${y.quasiStrList}"
    override def quasiquoteString:String = s"(${x.quasiquoteString}${y.quasiStrList}"
    override def toString:String = "`"+this.quasiquoteString
  }
  def cons(x:SExp,y:SExp) = Pair(x,y)
  private def seq2SExp(xs:Seq[SExp]):SExp= xs match {
    case head +: tail => cons(head, seq2SExp(tail))
    case Seq() => Empty
  }
  def list(xs:SExp*) = seq2SExp(xs)
  final case class Sym(x:Symbol) extends SExp with UnifiableAtom with ReadbackableAtom {
    override def quasiquoteString:String = x.name
    override def toString:String = "'"+this.quasiquoteString
  }
  object Sym {
    def apply(x:String):Sym=Sym(Symbol(x))
    def apply(x:Symbol):Sym=new Sym(x)
  }
  sealed trait Bool extends SExp with UnifiableAtom with ReadbackableAtom
  case object True extends Bool {
    override def toString:String = "#t"
  }
  val t = True
  case object False extends Bool {
    override def toString:String = "#f"
  }
  val f = False
  final case class SExpHole(x:Hole) extends SExp with Unifiable with Readbackable with UnifiableWrapper {
    override def unbox: Unifiable = x
    override def impl_unify(context: UnifyContext, other: Unifiable): UnifyResult = throw new UnsupportedOperationException()
    override def readback(context:UnifyContext):Any = x.readback(context) match {
      case x:Hole=> SExpHole(x)
      case x:SExp=>x
      case unexpected=>throw new IllegalStateException(unexpected.toString)
    }
      override def toString:String = x.toString
      override def quasiquoteString:String = ","+x.toString
  }
  implicit def hole2sexp(x:Hole):SExpHole = SExpHole(x)

  def printAll(x: SExpHole=>Goal): Set[SExp] = Set.empty.concat(Hole.fresh(q0=>{
    val q = hole2sexp(q0)
    x(q).runAll.map(ctx=>q.readback(ctx).asInstanceOf[SExp])}))
  def print1(x: SExpHole=>Goal): Option[SExp] = Hole.fresh(q0=>{
    val q = hole2sexp(q0)
    x(q).run1.map(_._1(0)).map(ctx=>q.readback(ctx).asInstanceOf[SExp])})
}

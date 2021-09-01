package skanren


import scala.annotation.targetName
import scala.collection.immutable.HashMap
import scala.collection.parallel.immutable.{ParHashMap, ParHashSet, ParVector, ParIterable}
import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

private val EMPTY_SYMBOL = Symbol("")

final class Hole[T](identifier: Symbol) {
  def this() = {
    this(EMPTY_SYMBOL)
  }
}

type SubstitutionStore = ParHashMap[Hole[_], _]

implicit class SubstitutionStoreOps(subst: SubstitutionStore) {

  private def get[T](x: Hole[T]): Option[T] = subst.get(x).asInstanceOf[Option[T]]

  private def updated[T](h: Hole[T], x: T): SubstitutionStore = subst.updated(h, x)

  def walk[T](x: T)(implicit unifier: Unifier[T]): T = (for {
    hole <- unifier.matchHole(x)
    next <- this.get(hole)
  } yield this.walk(next)).getOrElse(x)
}


type Unifying[T] = SubstitutionStore => Option[(SubstitutionStore, T)]

implicit class UnifyingOps[A](f: Unifying[A]) {
  def flatMap[B](x: A => Unifying[B]): Unifying[B] = st => f(st).flatMap({ case (st, a) => x(a)(st) })

  def map[B](x: A => B): Unifying[B] = st => f(st).map({ case (st, a) => (st, x(a)) })
}

object Unifying {
  def fail[A]: Unifying[A] = st => None

  def pure[A](x: A): Unifying[A] = st => Some((st, x))

  def guard(x: Boolean): Unifying[Unit] = if (x) Unifying.pure(()) else Unifying.fail
}

trait Unifier[T] {
  final implicit val thisUnifier: Unifier[T] = this

  def unify(self: T, other: T): Unifying[Unit] = ???

  def matchHole(x: T): Option[Hole[T]] = ???
}

trait ConcreteUnifier[T] extends Unifier[T] {
  def concreteUnify(self: T, other: T): Unifying[Unit] = ???

  override def matchHole(x: T): Option[Hole[T]] = None
}
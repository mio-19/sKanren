package skanren

sealed trait Hole[T]

final class UniqueHole[T](identifier: Symbol) extends Hole[T]

// TODO
final case class UnifyResult()

trait Unifiable {
  type T
  implicit protected val ev: T =:= this.type

  def unify(other: T): UnifyResult = ???

  protected def unifyConcrete(other: T): UnifyResult = throw new UnsupportedOperationException()

  def matchHole(x: T): Option[Hole[T]] = ???
}

implicit class UnifiablePatternMatching[T <: Unifiable](x: T) {
  def mat[R](clauses: T => R): R = ???
}

sealed trait Holeable[A] extends Unifiable {
  override type T = Holeable[A]
  override implicit protected val ev = implicitly
}

final case class HoleablePure[T](x: T)

final case class HoleableHole[T](x: Hole[T])

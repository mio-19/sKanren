package skanren

sealed trait SExp extends Unifiable

final case class SExpHole(hole: Hole[SExp]) extends SExp {
  override type T = SExp

  override def matchHole = Some(hole)
}

sealed trait SExpConcrete extends SExp with ConcreteUnifiable {
  override type T = SExpConcrete
}

final case class Atom(x: Symbol) extends SExpConcrete {
  override def unifyConcrete(other: T) = Unifying.guard(this == other)
}
package skanren

sealed trait SExp extends Unifiable

final case class SExpHole(hole: Hole[SExp]) extends SExp {
  override type T = SExp

  override def matchHole = Some(hole)
}

sealed trait SExpConcrete extends SExp with ConcreteUnifiable {
  override final type T = SExpConcrete
}

final case class Atom(x: Symbol) extends SExpConcrete {
  override def unifyConcrete(other: T) = Unifying.guard(this == other)
}

case object Empty extends SExpConcrete {
  override def unifyConcrete(other: T) = Unifying.guard(this == other)
}

sealed trait Bool extends SExpConcrete {
  override def unifyConcrete(other: T) = Unifying.guard(this == other)
}

case object True extends Bool

case object False extends Bool

final case class Pair(x: SExp, y: SExp) extends SExpConcrete {
  override def unifyConcrete(other: T) = other match {
    case Pair(a, b) => for {
      _ <- a.unify(x)
      _ <- b.unify(y)
    } yield ()
    case _ => Unifying.fail
  }
}

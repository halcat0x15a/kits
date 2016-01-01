package kits.free

sealed abstract class Union {

  type T

}

sealed abstract class Void extends Union

sealed abstract class :+:[F[_], U <: Union] extends Union

sealed abstract class Inl[F[_], U <: Union] extends (F :+: U) {

  def head: F[T]

}

object Inl {

  def apply[F[_], A, U <: Union](fa: F[A]): (F :+: U) { type T = A } =
    new Inl[F, U] {
      type T = A
      val head: F[A] = fa
    }

  def unapply[F[_], A, U <: Union](sum: (F :+: U) { type T = A }): Option[F[A]] =
    sum match {
      case inl: Inl[F, U] => Some(inl.head)
      case _ => None
    }

}

sealed abstract class Inr[F[_], U <: Union] extends (F :+: U) { self =>

  def tail: U { type T = self.T }

}

object Inr {

  def apply[F[_], A, U <: Union](u: U { type T = A }): (F :+: U) { type T = A } =
    new Inr[F, U]() {
      type T = A
      val tail: U { type T = A } = u
    }

  def unapply[F[_], A, U <: Union](sum: (F :+: U) { type T = A }): Option[U { type T = A }] =
    sum match {
      case inr: Inr[F, U] => Some(inr.tail)
      case _ => None
    }

}

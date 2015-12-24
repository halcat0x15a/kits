package kits.free

sealed abstract class Union

sealed abstract class Void extends Union

sealed abstract class :+:[F[_], U <: Union] extends Union {

  type T

}

sealed abstract case class Inl[F[_], U <: Union]() extends (F :+: U) {

  def head: F[T]

}

object Inl {

  def apply[F[_], A, U <: Union](fa: F[A]): (F :+: U) { type T = A } =
    new Inl[F, U] {
      type T = A
      val head: F[A] = fa
    }

}

case class Inr[F[_], U <: Union](tail: U) extends (F :+: U)

package kits.free

sealed abstract class Union { type T }

sealed abstract class Void extends Union

sealed abstract class :+:[F[_], U <: Union] extends Union

case class Inl[F[_], U <: Union, A](head: F[A]) extends (F :+: U) { type T = A }

case class Inr[F[_], U <: Union, A](tail: U { type T = A }) extends (F :+: U) { type T = A }

package kits.free

sealed abstract class Union { type T }

sealed abstract class Void extends Union

sealed abstract class :+:[F <: { type T }, U <: Union] extends Union

case class Inl[F <: { type T }, U <: Union, A](head: F { type T = A }) extends (F :+: U) { type T = A }

case class Inr[F <: { type T }, U <: Union, A](tail: U { type T = A }) extends (F :+: U) { type T = A }

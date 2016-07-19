package kits.free

sealed abstract class Union { type T }

sealed abstract class Void extends Union

sealed abstract class :+:[F, U] extends Union

case class Inl[F, U, A](head: F { type T = A }) extends (F :+: U) { type T = A }

case class Inr[F, U, A](tail: U { type T = A }) extends (F :+: U) { type T = A }

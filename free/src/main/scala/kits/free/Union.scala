package kits.free

sealed abstract class Union extends Product with Serializable

sealed abstract class Void extends Union

sealed abstract class :+:[F, U] extends Union

case class Inl[F, U](head: F) extends (F :+: U)

case class Inr[F, U](tail: U) extends (F :+: U)

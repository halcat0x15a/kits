package kits.generic

sealed trait Sum extends Any

sealed trait :+:[A, B <: Sum] extends Any with Sum

case class Left[A, B <: Sum](a: A) extends AnyVal with (A :+: B)

case class Right[A, B <: Sum](b: B) extends AnyVal with (A :+: B)

sealed trait Void extends Sum

case object Void extends Void

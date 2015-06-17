package kits.generic

sealed trait Sum

sealed trait :+:[A, B <: Sum] extends Sum

case class Left[A, B <: Sum](a: A) extends (A :+: B)

case class Right[A, B <: Sum](b: B) extends (A :+: B)

sealed trait Void extends Sum

case object Void extends Void

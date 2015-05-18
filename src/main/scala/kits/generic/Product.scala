package kits.generic

sealed trait Product

case class :*:[A, B <: Product](a: A, b: B) extends Product

sealed trait Unit extends Product

case object Unit extends Unit

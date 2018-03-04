package kits.eff

import scala.annotation.tailrec

class Arrs[-R, A, B] private (val vec: Vector[Any]) extends AnyVal {
  def :+[S, C](f: B => Eff[S, C]): Arrs[R with S, A, C] = new Arrs(vec :+ f)

  def ++[S, C](arrs: Arrs[S, B, C]): Arrs[R with S, A, C] = new Arrs(vec ++ arrs.vec)

  def apply(value: A): Eff[R, B] = {
    @tailrec
    def go(vec: Vector[Any => Eff[R, B]], value: Any): Eff[R, B] = {
      val h = vec.head
      val t = vec.tail
      h(value) match {
        case eff if t.isEmpty => eff
        case Eff.Pure(value) => go(t, value)
        case Eff.Impure(union, arrs) => Eff.Impure(union, arrs ++ new Arrs(t))
      }
    }
    go(vec.asInstanceOf[Vector[Any => Eff[R, B]]], value)
  }
}

object Arrs {
  def apply[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] = new Arrs(Vector(f))
}

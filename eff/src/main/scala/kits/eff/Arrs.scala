package kits.eff

import scala.annotation.tailrec

class Arrs[-R, A, B](val vec: Vector[Any]) extends AnyVal {
  def :+[S, C](f: B => Eff[S, C]): Arrs[R with S, A, C] = new Arrs(vec :+ f)

  def ++[S, C](arrs: Arrs[S, B, C]): Arrs[R with S, A, C] = new Arrs(vec ++ arrs.vec)

  def apply(value: A): Eff[R, B] = {
    @tailrec
    def loop(vec: Vector[Any => Eff[R, B]], value: Any): Eff[R, B] = {
      val f = vec.head
      val v = vec.tail
      f(value) match {
        case eff if v.isEmpty => eff
        case Eff.Pure(value) => loop(v, value)
        case Eff.Impure(union, arrs) => Eff.Impure(union, new Arrs(arrs.vec ++ v))
      }
    }
    loop(vec.asInstanceOf[Vector[Any => Eff[R, B]]], value)
  }
}

object Arrs {
  def apply[R, A, B](f: A => Eff[R, B]): Arrs[R, A, B] = new Arrs(Vector(f))
}

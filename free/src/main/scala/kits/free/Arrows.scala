package kits.free

import scala.annotation.tailrec

class Arrows[U, A, B] private (val vec: Vector[A => Free[U, B]]) extends AnyVal {

  def :+[C](f: B => Free[U, C]): Arrows[U, A, C] = new Arrows(coerce(vec :+ f))

  def ++[C](arrs: Arrows[U, B, C]): Arrows[U, A, C] = new Arrows(coerce(vec ++ arrs.vec))

  def apply(value: A): Free[U, B] = {
    @tailrec
    def go(vec: Vector[Any => Free[U, B]], value: Any): Free[U, B] = {
      val h = vec.head
      val t = vec.tail
      h(value) match {
        case free if t.isEmpty => free
        case Pure(value) => go(t, value)
        case Impure(union, arrs) => Impure(union, arrs ++ new Arrows(t))
      }
    }
    go(coerce(vec), value)
  }

  private[this] def coerce[A, B](vec: Vector[A]): Vector[B] = vec.asInstanceOf[Vector[B]]

}

object Arrows {

  def singleton[U, A, B](f: A => Free[U, B]): Arrows[U, A, B] = new Arrows(Vector(f))

}

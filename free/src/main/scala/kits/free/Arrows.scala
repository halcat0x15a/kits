package kits.free

import scala.annotation.tailrec

object Arrows {

  def singleton[U, A, B](f: A => Free[U, B]): Arrows[U, A, B] = Vector(f)

  def snoc[U, A, B, C](arrs: Arrows[U, A, B])(f: B => Free[U, C]): Arrows[U, A, C] = (arrs :+ f).asInstanceOf[Arrows[U, A, C]]

  def app[U, A, B](arrs: Arrows[U, A, B])(value: A): Free[U, B] = {
    @tailrec
    def go(arrs: Arrows[U, Any, B], value: Any): Free[U, B] =
      arrs match {
        case Vector(h) => h(value)
        case h +: t =>
          h(value) match {
            case Pure(value) => go(t, value)
            case Impure(union, arrs) => Impure(union, arrs ++ t)
          }
      }
    go(arrs.asInstanceOf[Arrows[U, Any, B]], value)
  }

}

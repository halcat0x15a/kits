package kits.free

import scala.annotation.tailrec

sealed abstract class Arrows[U <: Union, -A, +B] extends (A => Free[U, B]) {

  def apply(a: A): Free[U, B] = {
    @tailrec
    def go(arrows: Arrows[U, Any, B], value: Any): Free[U, B] =
      arrows.view match {
        case Arrows.One(h) => h(value)
        case Arrows.Cons(h, t) =>
          h(value) match {
            case Pure(value) => go(t, value)
            case Impure(union, arrows) => Impure(union, arrows ++ t)
          }
      }
    go(this.asInstanceOf[Arrows[U, Any, B]], a)
  }

  def :+[C](f: B => Free[U, C]): Arrows[U, A, C] = Arrows.Node(this, Arrows.Leaf(f))

  def ++[C](that: Arrows[U, B, C]): Arrows[U, A, C] = Arrows.Node(this, that)

  def view: Arrows.View[U, A, B]

}

object Arrows {

  def singleton[U <: Union, A, B](arrow: A => Free[U, B]): Arrows[U, A, B] = Leaf(arrow)

  case class Leaf[U <: Union, A, B](arrow: A => Free[U, B]) extends Arrows[U, A, B] {

    lazy val view: View[U, A, B] = One(arrow)

  }

  case class Node[U <: Union, A, B, C](left: Arrows[U, A, B], right: Arrows[U, B, C]) extends Arrows[U, A, C] {

    lazy val view: View[U, A, C] = {
      @tailrec
      def go(left: Arrows[U, A, Any], right: Arrows[U, Any, C]): View[U, A, C] =
        left match {
          case Leaf(h) => Cons(h, right)
          case Node(l, r) => go(l, Node(r, right))
        }
      go(left, right.asInstanceOf[Arrows[U, Any, C]])
    }

  }

  sealed abstract class View[U <: Union, -A, +B]
  
  case class One[U <: Union, A, B](head: A => Free[U, B]) extends View[U, A, B]
  
  case class Cons[U <: Union, A, B, C](head: A => Free[U, B], tail: Arrows[U, B, C]) extends View[U, A, C]

}

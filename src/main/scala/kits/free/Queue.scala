package kits.free

import scala.annotation.tailrec

sealed abstract class Queue[U <: Union, -A, +B] {

  def apply(a: A): Free[U, B] = {
    @tailrec
    def go(tpe: { type T })(arrows: Queue[U, tpe.T, B], value: tpe.T): Free[U, B] =
      arrows.view match {
        case One(f) => f(value)
        case cons@Cons() =>
          cons.head(value) match {
            case Pure(value) => go(new { type T = cons.T })(cons.tail, value)
            case impure@Impure() => Impure(impure.union, impure.arrows ++ cons.tail)
          }
      }
    go(new { type T = A })(this, a)
  }

  def :+[C](f: B => Free[U, C]): Queue[U, A, C] = Node(this, Leaf(f))

  def ++[C](that: Queue[U, B, C]): Queue[U, A, C] = Node(this, that)

  def view: View[U, A, B]

}

case class Leaf[U <: Union, A, B](arrow: A => Free[U, B]) extends Queue[U, A, B] {

  lazy val view: View[U, A, B] = One(arrow)

}

sealed abstract case class Node[U <: Union, A, B]() extends Queue[U, A, B] { self =>

  type T

  def left: Queue[U, A, T]

  def right: Queue[U, T, B]

  lazy val view: View[U, A, B] = {
    @tailrec
    def go[U <: Union, A, B](tpe: { type T })(left: Queue[U, A, tpe.T], right: Queue[U, tpe.T, B]): View[U, A, B] =
      left match {
        case Leaf(value) => Cons(value, right)
        case node@Node() => go[U, A, B](new { type T = node.T })(node.left, Node(node.right, right))
      }
    go(new { type T = self.T })(left, right)
  }

}

object Node {

  def apply[U <: Union, A, B, C](f: Queue[U, A, B], g: Queue[U, B, C]): Node[U, A, C] { type T = B } =
    new Node[U, A, C] {
      type T = B
      val left: Queue[U, A, B] = f
      val right: Queue[U, B, C] = g
    }

}

sealed abstract class View[U <: Union, -A, +B]

case class One[U <: Union, A, B](head: A => Free[U, B]) extends View[U, A, B]

sealed abstract case class Cons[U <: Union, A, B]() extends View[U, A, B] {

  type T

  def head: A => Free[U, T]

  def tail: Queue[U, T, B]

}

object Cons {

  def apply[U <: Union, A, B, C](h: A => Free[U, B], t: Queue[U, B, C]): Cons[U, A, C] { type T = B } =
    new Cons[U, A, C] {
      type T = B
      val head: A => Free[U, B] = h
      val tail: Queue[U, B, C] = t
    }

}

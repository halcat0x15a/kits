package kits.free

import scala.annotation.tailrec

sealed abstract class Arrows[U <: Union, -A, +B] {

  import Arrows._

  def apply(a: A): Free[U, B] = {
    @tailrec
    def go(tpe: { type T })(arrows: Arrows[U, tpe.T, B], value: tpe.T): Free[U, B] =
      arrows.view match {
        case One(arrow) => arrow(value)
        case cons@Cons() =>
          cons.head(value) match {
            case Pure(value) => go(new { type T = cons.T })(cons.tail, value)
            case Impure(union, arrows) => Impure(union, arrows ++ cons.tail)
          }
      }
    go(new { type T = A })(this, a)
  }

  def :+[C](f: B => Free[U, C]): Arrows[U, A, C] = Node(this, Leaf(f))

  def ++[C](that: Arrows[U, B, C]): Arrows[U, A, C] = Node(this, that)

  def view: View[U, A, B]

}

object Arrows {

  def singleton[U <: Union, A, B](arrow: A => Free[U, B]): Arrows[U, A, B] = Leaf(arrow)

  case class Leaf[U <: Union, A, B](arrow: A => Free[U, B]) extends Arrows[U, A, B] {

    lazy val view: View[U, A, B] = One(arrow)

  }

  sealed abstract case class Node[U <: Union, A, B]() extends Arrows[U, A, B] { self =>

    type T

    def left: Arrows[U, A, T]

    def right: Arrows[U, T, B]

    lazy val view: View[U, A, B] = {
      @tailrec
      def go[U <: Union, A, B](tpe: { type T })(left: Arrows[U, A, tpe.T], right: Arrows[U, tpe.T, B]): View[U, A, B] =
        left match {
          case Leaf(arrow) => Cons(arrow, right)
          case node@Node() => go[U, A, B](new { type T = node.T })(node.left, Node(node.right, right))
        }
      go(new { type T = self.T })(left, right)
    }

  }

  object Node {

    def apply[U <: Union, A, B, C](l: Arrows[U, A, B], r: Arrows[U, B, C]): Node[U, A, C] { type T = B } =
      new Node[U, A, C] {
        type T = B
        val left: Arrows[U, A, B] = l
        val right: Arrows[U, B, C] = r
      }

  }

  sealed abstract class View[U <: Union, -A, +B]
  
  case class One[U <: Union, A, B](head: A => Free[U, B]) extends View[U, A, B]
  
  sealed abstract case class Cons[U <: Union, A, B]() extends View[U, A, B] {
  
    type T
  
    def head: A => Free[U, T]
  
    def tail: Arrows[U, T, B]
  
  }
  
  object Cons {
  
    def apply[U <: Union, A, B, C](h: A => Free[U, B], t: Arrows[U, B, C]): Cons[U, A, C] { type T = B } =
      new Cons[U, A, C] {
        type T = B
        val head: A => Free[U, B] = h
        val tail: Arrows[U, B, C] = t
      }
  
  }

}

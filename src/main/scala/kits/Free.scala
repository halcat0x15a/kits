package kits.free

import scala.annotation.tailrec

sealed abstract class Queue[R[_], -A, +B] {

  def :+[C](f: B => Free[R, C]): Queue[R, A, C] = Node(this, Leaf(f))

  def ++[C](that: Queue[R, B, C]): Queue[R, A, C] = Node(this, that)

  def view: View[R, A, B]

}

case class Leaf[R[_], A, B](arrow: A => Free[R, B]) extends Queue[R, A, B] {

  lazy val view: View[R, A, B] = One(arrow)

}

sealed abstract case class Node[R[_], A, B]() extends Queue[R, A, B] { self =>

  type T

  def left: Queue[R, A, T]

  def right: Queue[R, T, B]

  lazy val view: View[R, A, B] = {
    @tailrec
    def go[R[_], A, B](tpe: { type T })(left: Queue[R, A, tpe.T], right: Queue[R, tpe.T, B]): View[R, A, B] =
      left match {
        case Leaf(value) => Cons(value, right)
        case node@Node() => go[R, A, B](new { type T = node.T })(node.left, Node(node.right, right))
      }
    go(new { type T = self.T })(left, right)
  }

}

object Node {

  def apply[R[_], A, B, C](f: Queue[R, A, B], g: Queue[R, B, C]): Node[R, A, C] { type T = B } =
    new Node[R, A, C] {
      type T = B
      val left = f
      val right = g
    }

}

sealed abstract class View[R[_], -A, +B]

case class One[R[_], A, B](arrow: A => Free[R, B]) extends View[R, A, B]

sealed abstract case class Cons[R[_], A, B]() extends View[R, A, B] {

  type T

  def arrow: A => Free[R, T]

  def arrows: Queue[R, T, B]

}

object Cons {

  def apply[R[_], A, B, C](head: A => Free[R, B], tail: Queue[R, B, C]): Cons[R, A, C] { type T = B } =
    new Cons[R, A, C] {
      type T = B
      val arrow = head
      val arrows = tail
    }

}

sealed abstract class Sum

sealed abstract class Void extends Sum

sealed abstract class :+:[F[_], G <: Sum] {

  type T

}

case class L[F[_], A, G <: Sum](value: F[A]) extends (F :+: G) {

  type T = A

}

case class R[F[_], A, G <: Sum](value: G) extends (F :+: G) {

  type T = A

}

sealed abstract class Free[R[_], +A] {

  def map[B](f: A => B): Free[R, B]

  def flatMap[B](f: A => Free[R, B]): Free[R, B]
  
  def fold[B](f: A => Free[R, B])(g: R[Any] => (Any => Free[R, B]) => Free[R, B]): B = {
    def go(free: Free[R, A]): Free[R, B] =
      free match {
        case Pure(v) => f(v)
        case impure@Impure() =>
          g(impure.union.asInstanceOf[R[Any]])(x => go(Free(impure.arrows.asInstanceOf[Queue[R, Any, A]], x)))
      }
    go(this) match {
      case Pure(a) => a
    }
  }

}

case class Pure[R[_], A](value: A) extends Free[R, A] {

  def map[B](f: A => B): Free[R, B] = Pure(f(value))

  def flatMap[B](f: A => Free[R, B]): Free[R, B] = f(value)

}

sealed abstract case class Impure[R[_], A]() extends Free[R, A] {

  type T

  def union: R[T]

  def arrows: Queue[R, T, A]

  def map[B](f: A => B): Free[R, B] = Impure(union, arrows :+ (a => Pure(f(a))))

  def flatMap[B](f: A => Free[R, B]): Free[R, B] = Impure(union, arrows :+ f)

}

object Impure {

  def apply[R[_], A, B](u: R[A], a: Queue[R, A, B]): Impure[R, B] { type T = A } =
    new Impure[R, B] {
      type T = A
      def union = u
      def arrows = a
    }

}

object Free {

  def apply[R[_], A, B](arrows: Queue[R, A, B], value: A): Free[R, B] = {
    @tailrec
    def go[R[_], A](tpe: { type T })(arrows: Queue[R, tpe.T, A], value: tpe.T): Free[R, A] =
      arrows.view match {
        case One(f) => f(value)
        case cons@Cons() =>
          cons.arrow(value) match {
            case Pure(value) => go[R, A](new { type T = cons.T })(cons.arrows, value)
            case impure@Impure() => Impure(impure.union, impure.arrows ++ cons.arrows)
          }
      }
    go(new { type T = A })(arrows, value)
  }

}

sealed abstract class Reader[A]

case class Get() extends Reader[String]

object Reader {

  def run[A](free: Free[Reader, A], str: String): A =
    free.fold(x => Pure(x))(_ => k => k(str))

}

sealed abstract class Writer[+A]

case class Put(value: String) extends Writer[Unit]

object Writer {

  def run[A](free: Free[Writer, A]): (A, List[String]) =
    free.fold(a => Pure((a, List.empty[String]))) {
      case Put(w) => k => k(()).map { case (a, l) => (a, w :: l) }
    }

  def tell(value: String): Free[Writer, Unit] =
    Impure(Put(value), Leaf(Pure(_: Any)))

}

object Example extends App {

  val e1 = for {
    _ <- Writer.tell("hoge")
    _ <- Writer.tell("hoge")
    _ <- Writer.tell("hoge")
  } yield ()

  def e2(n: Int): Free[Writer, Unit] =
    if (n <= 0)
      Writer.tell("end")
    else
      for {
        _ <- Writer.tell("hoge")
        _ <- e2(n - 1)
      } yield ()

  println(Writer.run(e1))
  println(Writer.run(e2(100000))._2.size)

}

package kits.free

import scala.annotation.tailrec

sealed abstract class Queue[R[_], -A, +B] {

  def :+[C](f: B => Free[R, C]): Queue[R, A, C] = new Node(this, new Leaf(f))

  def ++[C](that: Queue[R, B, C]): Queue[R, A, C] = new Node(this, that)

  def view: View[R, A, B]

}

case class Leaf[R[_], A, B](arrow: A => Free[R, B]) extends Queue[R, A, B] {

  lazy val view: View[R, A, B] = new One(arrow)

}

case class Node[R[_], A, B, T](left: Queue[R, A, T], right: Queue[R, T, B]) extends Queue[R, A, B] {

  lazy val view: View[R, A, B] = {
    @tailrec
    def go[R[_], A, B](x: Queue[R, A, Any], y: Queue[R, Any, B]): View[R, A, B] =
      x match {
        case Leaf(v) => new Cons(v, y)
        case Node(l, r) => go(l, new Node(r, y))
      }
    go(left, right.asInstanceOf[Queue[R, Any, B]])
  }

}

sealed abstract class View[R[_], -A, +B]

case class One[R[_], A, B](arrow: A => Free[R, B]) extends View[R, A, B]

case class Cons[R[_], A, B, T](arrow: A => Free[R, T], queue: Queue[R, T, B]) extends View[R, A, B]

sealed abstract class Free[R[_], +A] {

  def map[B](f: A => B): Free[R, B]

  def flatMap[B](f: A => Free[R, B]): Free[R, B]

}

case class Pure[R[_], A](value: A) extends Free[R, A] {

  def map[B](f: A => B): Free[R, B] = new Pure(f(value))

  def flatMap[B](f: A => Free[R, B]): Free[R, B] = f(value)

}

case class Impure[R[_], A, T](union: R[T], arrows: Queue[R, T, A]) extends Free[R, A] {

  def map[B](f: A => B): Free[R, B] = new Impure(union, arrows :+ (a => new Pure(f(a))))

  def flatMap[B](f: A => Free[R, B]): Free[R, B] = new Impure(union, arrows :+ f)

}

object Free {

  @tailrec
  def apply[R[_], A](arrows: Queue[R, Any, A], value: Any): Free[R, A] =
    arrows.view match {
      case One(f) => f(value)
      case Cons(f, t) =>
        f(value) match {
          case Pure(v) => apply(t, v)
          case Impure(u, a) => Impure(u, (t ++ a).asInstanceOf[Queue[R, Any, A]])
        }
    }

  def fold[R[_], A, B](free: Free[R, A])(f: A => B)(g: R[Any] => (Any => B) => B): B =
    free match {
      case Pure(v) => f(v)
      case Impure(u, a) => g(u)(x => fold(apply(a, x))(f)(g))
    }

}

case class Writer[T](value: String)

object Writer {

  def run(free: Free[Writer, Any]): (Any, List[String]) = {
    @tailrec
    def go(free: Free[Writer, Any], acc: List[String]): (Any, List[String]) =
      free match {
        case Pure(v) => (v, acc)
        case Impure(Writer(s), a) => go(Free(a, ()), s :: acc)
      }
    go(free, Nil)
  }

  def runF(free: Free[Writer, Any]): (Any, List[String]) =
    Free.fold(free)(a => (a, List.empty[String])) {
      case Writer(s) => k => k(()) match {
        case (a, l) => (a, s :: l)
      }
    }

  def tell(value: String): Free[Writer, Unit] =
    Impure(Writer(value), Leaf(Pure(_: Any)))

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
  println(Writer.run(e2(10000))._2.size)

}

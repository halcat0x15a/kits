package kits.free

import scala.annotation.tailrec

sealed abstract class Union {

  type T

}

sealed abstract class Void extends Union

sealed abstract class :+:[F[_], U <: Union] extends Union

case class Inl[F[_], A, U <: Union](value: F[A]) extends (F :+: U) {

  type T = A

}

case class Inr[F[_], A, U <: Union](value: U) extends (F :+: U) {

  type T = A

}

sealed abstract class Member[F[_], U <: Union] {

  def inject[A](fa: F[A]): U

  def project[A](u: U { type T = A }): Option[F[A]]

}

object Member {

  implicit def left[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](fa: F[A]): F :+: U = Inl(fa)
      def project[A](u: (F :+: U) { type T = A }): Option[F[A]] =
        u match {
          case Inl(v) => Some(v)
          case Inr(_) => None
        }
    }

  implicit def right[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](fa: F[A]): G :+: U = Inr(member.inject(fa))
      def project[A](u: (G :+: U) { type T = A }): Option[F[A]] =
        u match {
          case Inl(_) => None
          case Inr(u: U { type T = A }) => member.project(u)
        }
    }

}

sealed abstract class Queue[U <: Union, -A, +B] {

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
      val left = f
      val right = g
    }

}

sealed abstract class View[U <: Union, -A, +B]

case class One[U <: Union, A, B](arrow: A => Free[U, B]) extends View[U, A, B]

sealed abstract case class Cons[U <: Union, A, B]() extends View[U, A, B] {

  type T

  def arrow: A => Free[U, T]

  def arrows: Queue[U, T, B]

}

object Cons {

  def apply[U <: Union, A, B, C](head: A => Free[U, B], tail: Queue[U, B, C]): Cons[U, A, C] { type T = B } =
    new Cons[U, A, C] {
      type T = B
      val arrow = head
      val arrows = tail
    }

}

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

  def run(implicit ev: U =:= Void): A =
    this match {
      case Pure(a) => a
    }
  
  def fold[B](f: A => Free[U, B])(g: Any => (Any => Free[U, B]) => Free[U, B]): Free[U, B] = {
    def go(free: Free[U, A]): Free[U, B] =
      free match {
        case Pure(v) => f(v)
        case impure@Impure(u, a) =>
          def k(x: Any): Free[U, B] =
            go(Free(impure.arrows.asInstanceOf[Queue[U, Any, A]], x))
          impure.union match {
            case Inl(v) => g(v)(k)
            case Inr(u: U) => Impure(u, Leaf(k))
          }
      }
    go(this)
  }

}

case class Pure[U <: Union, A](value: A) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Pure(f(value))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = f(value)

}

case class Impure[U <: Union, A](union: U, arrows: Queue[U, U#T, A]) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Impure(union, arrows :+ (a => Pure(f(a))))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = Impure(union, arrows :+ f)

}

object Free {

  def apply[U <: Union, A, B](arrows: Queue[U, A, B], value: A): Free[U, B] = {
    @tailrec
    def go[U <: Union, A](tpe: { type T })(arrows: Queue[U, tpe.T, A], value: tpe.T): Free[U, A] =
      arrows.view match {
        case One(f) => f(value)
        case cons@Cons() =>
          cons.arrow(value) match {
            case Pure(value) => go[U, A](new { type T = cons.T })(cons.arrows, value)
            case Impure(union, arrows) => Impure(union, arrows ++ cons.arrows)
          }
      }
    go(new { type T = A })(arrows, value)
  }

}

sealed abstract class Reader[A]

case class Get() extends Reader[String]

object Reader {

  @tailrec
  def run[A](free: Free[Reader :+: Void, A], str: String): A =
    free match {
      case Pure(a) => a
      case impure@Impure(_, a) => run[A](Free(a, str.asInstanceOf[impure.union.T]), str)
    }

  def ask[U <: Union](implicit member: Member[Reader, U]): Free[U, String] =
    Impure(member.inject(Get()), Leaf(x => Pure(x.asInstanceOf[String])))

}

sealed abstract class Writer[+A]

case class Put(value: String) extends Writer[Unit]

object Writer {

  def run[U <: Union, A](free: Free[Writer :+: U, A]): Free[U, (A, List[String])] = {
    @tailrec
    def go(free: Free[Writer :+: U, A], acc: List[String]): Free[U, (A, List[String])] =
      free match {
        case Pure(a) => Pure((a, acc))
        case impure@Impure(_, _) =>
          impure.union match {
            case Inl(Put(w)) => go(Free(impure.arrows, ().asInstanceOf[impure.union.T]), w :: acc)
            case Inr(u) => ???
          }
      }
    go(free, Nil)
  }

  def tell[U <: Union](value: String)(implicit member: Member[Writer, U]): Free[U, Unit] =
    Impure(member.inject(Put(value)), Leaf(Pure(_)))

}

object Example extends App {

  def e1[U <: Union](implicit w: Member[Writer, U], r: Member[Reader, U]): Free[U, Unit] = for {
    _ <- Writer.tell("start")
    a <- Reader.ask
    _ <- Writer.tell(a)
    _ <- Writer.tell("end")
  } yield ()

  def e2[U <: Union](n: Int)(implicit member: Member[Writer, U]): Free[U, Unit] =
    if (n <= 0)
      Writer.tell("end")
    else
      for {
        _ <- Writer.tell("hoge")
        _ <- e2(n - 1)
      } yield ()

  println(Reader.run(Writer.run(e1), "hoge"))
  println(Writer.run(e2[Writer :+: Void](10000)).run._2.size)

}

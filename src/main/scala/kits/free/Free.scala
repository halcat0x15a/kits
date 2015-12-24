package kits.free

import scala.annotation.tailrec

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

}

case class Pure[U <: Union, A](value: A) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Pure(f(value))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = f(value)

}

sealed abstract case class Impure[U <: Union, A]() extends Free[U, A] {

  type T

  def union: U

  def arrows: Queue[U, T, A]

  def map[B](f: A => B): Free[U, B] = Impure(union, arrows :+ (a => Pure(f(a))))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = Impure(union, arrows :+ f)

}

object Impure {

  def apply[U <: Union, A, B](u: U, a: Queue[U, A, B]): Impure[U, B] { type T = A } =
    new Impure[U, B] {
      type T = A
      val union: U = u
      val arrows: Queue[U, A, B] = a
    }

}

object Free {

  def run[A](free: Free[Void, A]): A =
    free match {
      case Pure(a) => a
    }

  trait Fold[F[_], U <: Union, A] {
    def apply[T](fa: F[T])(f: T => Free[Trampoline :+: U, A]): Free[Trampoline :+: U, A]
  }

  def fold[F[_], U <: Union, A, B, T](free: Free[F :+: U, A])(f: A => Free[Trampoline :+: U, B])(g: Fold[F, U, B]): Free[U, B] = {
    def go(free: Free[F :+: U, A]): Free[Trampoline :+: U, B] =
      free match {
        case Pure(v) => f(v)
        case impure@Impure() =>
          def k[A](x: A): Free[Trampoline :+: U, B] = Trampoline(go(impure.arrows(x.asInstanceOf[impure.T])))
          impure.union match {
            case inl@Inl() => g(inl.head)(k)
            case Inr(u) => Impure(Inr(u), Leaf(k))
          }
      }
    Trampoline.run(go(free))
  }

}

object Example extends App {

  def e1[U <: Union](implicit w: Member[Writer, U], r: Member[Reader, U]): Free[U, Unit] = for {
    _ <- Writer.tell("start")
    a <- Reader.ask[U, String]
    _ <- Writer.tell(a)
    _ <- Writer.tell("end")
  } yield ()

  def e2[U <: Union](n: Int)(implicit member: Member[Writer, U], r: Member[Reader, U]): Free[U, Unit] =
    if (n <= 0) {
      Writer.tell("end")
    } else {
      for {
        a <- Reader.ask[U, String]
        _ <- Writer.tell(a)
        _ <- e2(n - 1)
      } yield ()
    }

  println(Free.run(Reader.run(Writer.run(e1[Writer :+: Reader :+: Void]), "hoge")))
  println(Free.run(Writer.run(Reader.run(e2[Reader :+: Writer :+: Void](100000), "hoge")))._2.size)

}

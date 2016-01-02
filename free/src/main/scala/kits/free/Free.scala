package kits.free

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

}

case class Pure[U <: Union, A](value: A) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Pure(f(value))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = f(value)

}

case class Impure[U <: Union, A, B](union: U { type T = A }, arrows: Arrows[U, A, B]) extends Free[U, B] {

  def map[C](f: B => C): Free[U, C] = Impure(union, arrows :+ (x => Pure(f(x))))

  def flatMap[C](f: B => Free[U, C]): Free[U, C] = Impure(union, arrows :+ f)

}

object Free {

  def apply[F[_], A, U <: Union](fa: F[A])(implicit member: Member[F, U]): Free[U, A] =
    Impure(member.inject(fa), Arrows.singleton(Pure(_: A)))

  def run[A](free: Free[Void, A]): A =
    (free: @unchecked) match {
      case Pure(a) => a
    }

}

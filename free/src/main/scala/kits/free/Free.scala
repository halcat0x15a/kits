package kits

package free

import scala.annotation.tailrec

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

case class Lazy[U <: Union, A](free: () => Free[U, A]) extends Free[U, A] {

  def map[B](f: A => B): Free[U, B] = Lazy(() => free().map(f))

  def flatMap[B](f: A => Free[U, B]): Free[U, B] = Lazy(() => free().flatMap(f))

}

object Free {

  def apply[F <: { type T }, A, U <: Union](union: U { type T = A }): Free[U, A] = Impure(union, Arrows.singleton(Pure(_: A)))

  @tailrec
  def run[A](free: Free[Void, A]): A =
    (free: @unchecked) match {
      case Pure(a) => a
      case Lazy(f) => run(f())
    }

  @tailrec
  def resume[U <: Union, A](free: Free[U, A]): Free[U, A] =
    free match {
      case Pure(_) => free
      case Impure(_, _) => free
      case Lazy(f) => resume(f())
    }

  def fold[F <: { type T }, A, B, U <: Union](free: Free[F :+: U, A])(f: A => Free[U, B])(g: F => (Any => Free[U, B]) => Free[U, B]): Free[U, B] = accum(free, ())((a, _) => f(a))(fa => _ => k => g(fa)(a => k(a, ())))

  def accum[F <: { type T }, A, B, S, U <: Union](free: Free[F :+: U, A], state: S)(f: (A, S) => Free[U, B])(g: F => S => ((Any, S) => Free[U, B]) => Free[U, B]): Free[U, B] =
    (resume(free): @unchecked) match {
      case Pure(a) => f(a, state)
      case Impure(Inl(fa), k) => g(fa)(state)((x, s) => Lazy(() => accum(k(x), s)(f)(g)))
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => accum(k(x), state)(f)(g)))
    }

  def intercept[F <: { type T }, A, B, U <: Union](free: Free[U, A])(f: A => Free[U, B])(g: F => (Any => Free[U, B]) => Free[U, B])(implicit F: Member[F, U]): Free[U, B] =
    (resume(free): @unchecked) match {
      case Pure(a) => f(a)
      case Impure(u, k) =>
        F.project(u) match {
          case Some(fa) => g(fa)(x => intercept(k(x))(f)(g))
          case None => Impure(u, Arrows.singleton((x: Any) => intercept(k(x))(f)(g)))
        }
    }

  implicit def FreeMonad[U <: Union]: Monad[({ type F[A] = Free[U, A] })#F] =
    new Monad[({ type F[A] = Free[U, A] })#F] {
      def pure[A](a: A): Free[U, A] = Pure(a)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

}

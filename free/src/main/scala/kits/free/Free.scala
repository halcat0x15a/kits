package kits

package free

import scala.annotation.tailrec

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

  def withFilter[M[_]](p: A => Boolean)(implicit F: Member[Choice[M], U]): Free[U, A] = flatMap(a => if (p(a)) Pure(a) else Choice.zero)

  @tailrec
  final def resume: Free[U, A] =
    this match {
      case Free.Lazy(f) => f().resume
      case _ => this
    }

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

  case class Lazy[U <: Union, A](free: () => Free[U, A]) extends Free[U, A] {

    def map[B](f: A => B): Free[U, B] = Lazy(() => free().map(f))

    def flatMap[B](f: A => Free[U, B]): Free[U, B] = Lazy(() => free().flatMap(f))

  }

  def apply[U <: Union, A](union: U { type T = A }): Free[U, A] = Impure(union, Arrows.singleton(Pure(_: A)))

  def delay[U <: Union, A](free: => Free[U, A]): Free[U, A] = {
    lazy val f = free
    Lazy(() => f)
  }

  def run[A](free: Free[Void, A]): A =
    (free.resume: @unchecked) match {
      case Pure(a) => a
    }

  def handleRelay[F <: { type T }, A, B, U <: Union](free: Free[F :+: U, A])(f: A => Free[U, B])(g: F => (Any => Free[U, B]) => Free[U, B]): Free[U, B] =
    handleRelay(free, ())((a, _) => f(a))(fa => _ => k => g(fa)(a => k(a, ())))

  def handleRelay[F <: { type T }, A, B, S, U <: Union](free: Free[F :+: U, A], state: S)(f: (A, S) => Free[U, B])(g: F => S => ((Any, S) => Free[U, B]) => Free[U, B]): Free[U, B] =
    (free.resume: @unchecked) match {
      case Pure(a) => f(a, state)
      case Impure(Inl(fa), k) => g(fa)(state)((x, s) => delay(handleRelay(k(x), s)(f)(g)))
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => handleRelay(k(x), state)(f)(g)))
    }

  def interpose[F <: { type T }, A, B, U <: Union](free: Free[U, A])(f: A => Free[U, B])(g: F => (Any => Free[U, B]) => Free[U, B])(implicit F: Member[F, U]): Free[U, B] =
    (free.resume: @unchecked) match {
      case Pure(a) => f(a)
      case Impure(u, k) =>
        F.project(u) match {
          case Some(fa) => g(fa)(x => delay(interpose(k(x))(f)(g)))
          case None => Impure(u, Arrows.singleton((x: Any) => interpose(k(x))(f)(g)))
        }
    }

  implicit def FreeMonad[U <: Union]: Monad[({ type F[A] = Free[U, A] })#F] =
    new Monad[({ type F[A] = Free[U, A] })#F] {
      def pure[A](a: A): Free[U, A] = Pure(a)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

  implicit def FreeMonadPlus[U <: Union, M[_]](implicit F: Member[Choice[M], U]): MonadPlus[({ type F[A] = Free[U, A] })#F] =
    new MonadPlus[({ type F[A] = Free[U, A] })#F] {
      def zero[A]: Free[U, A] = Choice.zero
      def pure[A](a: A): Free[U, A] = Pure(a)
      def plus[A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Choice.plus(x, y)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

}

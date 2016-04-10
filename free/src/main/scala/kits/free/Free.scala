package kits

package free

import scala.annotation.tailrec

sealed abstract class Free[U <: Union, +A] {

  def map[B](f: A => B): Free[U, B]

  def flatMap[B](f: A => Free[U, B]): Free[U, B]

  def withFilter[M[_]](p: A => Boolean)(implicit F: Member[Choice[M], U]): Free[U, A] = flatMap(a => if (p(a)) Pure(a) else Choice.zero)

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

  def apply[U <: Union, A](union: U { type T = A }): Free[U, A] = Impure(union, Arrows.singleton(Pure(_: A)))

  def run[A](free: Free[Void, A]): A =
    (free: @unchecked) match {
      case Pure(a) => a
    }

  def handleRelay[F <: { type T }, U <: Union, A, B, S](free: Free[F :+: U, A], state: S)(f: (A, S) => Either[(Free[F :+: U, A], S), B])(g: (F, S) => (Any => Free[F :+: U, A]) => Either[(Free[F :+: U, A], S), Free[U, B]]): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a, state) match {
          case Right(b) => Pure(b)
          case Left((free, state)) => handleRelay(free, state)(f)(g)
        }
      case Impure(Inl(fa), k) =>
        g(fa, state)(k) match {
          case Right(free) => free
          case Left((free, state)) => handleRelay(free, state)(f)(g)
        }
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => handleRelay(k(x), state)(f)(g)))
    }

  def handleRelay[F <: { type T }, U <: Union, A, B](free: Free[F :+: U, A])(f: A => B)(g: F => (Any => Free[F :+: U, A]) => Either[Free[F :+: U, A], Free[U, B]]): Free[U, B] = handleRelay(free, ())((a, _) => Right(f(a)))((fa, _) => k => g(fa)(k).left.map(a => (a, ())))

  def interpose[F <: { type T }, U <: Union, A, B, S](free: Free[U, A], state: S)(f: (A, S) => Either[(Free[U, A], S), B])(g: (F, S) => (Any => Free[U, A]) => Either[(Free[U, A], S), Free[U, B]])(implicit F: Member[F, U]): Free[U, B] =
    free match {
      case Pure(a) =>
        f(a, state) match {
          case Right(b) => Pure(b)
          case Left((free, state)) => interpose(free, state)(f)(g)
        }
      case Impure(u, k) =>
        F.project(u) match {
          case Some(fa) =>
            g(fa, state)(k) match {
              case Right(free) => free
              case Left((free, state)) => interpose(free, state)(f)(g)
            }
          case None => Impure(u, Arrows.singleton((x: Any) => interpose(k(x), state)(f)(g)))
        }
    }

  def interpose[F <: { type T }, U <: Union, A, B](free: Free[U, A])(f: A => B)(g: F => (Any => Free[U, A]) => Either[Free[U, A], Free[U, B]])(implicit F: Member[F, U]): Free[U, B] = interpose(free, ())((a, _) => Right(f(a)))((fa: F, _) => k => g(fa)(k).left.map(a => (a, ())))

  implicit def Monad[U <: Union]: Monad[({ type F[A] = Free[U, A] })#F] =
    new Monad[({ type F[A] = Free[U, A] })#F] {
      def pure[A](a: A): Free[U, A] = Pure(a)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

  implicit def MonadPlus[U <: Union: Choice[M]#Member, M[_]]: MonadPlus[({ type F[A] = Free[U, A] })#F] =
    new MonadPlus[({ type F[A] = Free[U, A] })#F] {
      def zero[A]: Free[U, A] = Choice.zero
      def pure[A](a: A): Free[U, A] = Pure(a)
      def plus[A](x: Free[U, A], y: Free[U, A]): Free[U, A] = Choice.plus(x, y)
      override def map[A, B](fa: Free[U, A])(f: A => B): Free[U, B] = fa.map(f)
      def flatMap[A, B](fa: Free[U, A])(f: A => Free[U, B]): Free[U, B] = fa.flatMap(f)
    }

}

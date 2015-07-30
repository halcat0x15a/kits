package kits

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

}

object Monad {

  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  case class Reader[F[_], R, A](value: R => F[A]) extends AnyVal

  case class Writer[F[_], W, A](value: F[(W, A)]) extends AnyVal

  case class State[F[_], S, A](value: S => F[(S, A)]) extends AnyVal

}

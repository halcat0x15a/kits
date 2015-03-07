package kits

trait Applicative[F[_]] extends Functor[F] {
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))
}

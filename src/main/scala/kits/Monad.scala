package kits

trait Monad[F[_]] extends Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)
}

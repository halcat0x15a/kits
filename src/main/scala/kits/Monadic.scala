package kits

trait Monadic[F[_]] extends Functor[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
  def flatten[A](fa: F[F[A]]): F[A] = flatMap(fa)(identity)
}

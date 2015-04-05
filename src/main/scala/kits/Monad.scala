package kits

trait Monad[F[_]] extends Any with Applicative[F] {
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))
  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))
}

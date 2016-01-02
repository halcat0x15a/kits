package kits

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}

object Monad {

  def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

  def flatten[F[_], A](ffa: F[F[A]])(implicit F: Monad[F]): F[A] = F.flatten(ffa)

}

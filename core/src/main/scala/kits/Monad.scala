package kits

trait Monad[F[_]] extends Applicative[F] {

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}

object Monad {

  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit F: Monad[F]): F[B] = F.flatMap(fa)(f)

  def flatten[F[_], A](ffa: F[F[A]])(implicit F: Monad[F]): F[A] = F.flatten(ffa)

  implicit class Ops[F[_], A](val self: F[A])(implicit F: Monad[F]) {

    def map[B](f: A => B): F[B] = F.map(self)(f)

    def flatMap[B](fa: F[A])(f: A => F[B]): F[B] = F.flatMap(self)(f)

  }

}

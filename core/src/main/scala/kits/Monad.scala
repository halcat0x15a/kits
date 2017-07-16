package kits

import kits.std._

trait Monad[F[_]] extends Applicative[F] { F =>

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

}

object Monad extends MonadFunctions[Monad] {

  implicit def Either[E]: Monad[({ type F[A] = Either[E, A] })#F] = new EitherMonad[E] with EitherFunctor[E] {}

}

trait MonadFunctions[T[F[_]] <: Monad[F]] extends ApplicativeFunctions[T] {

  def flatMap[F[_], A, B](fa: F[A])(f: A => F[B])(implicit F: T[F]): F[B] = F.flatMap(fa)(f)

  def flatten[F[_], A](ffa: F[F[A]])(implicit F: T[F]): F[A] = F.flatten(ffa)

}

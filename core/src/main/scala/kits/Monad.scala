package kits

import scala.language.implicitConversions

trait Monad[F[_]] extends Applicative[F] { F =>

  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(a => pure(f(a)))

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  def flatten[A](ffa: F[F[A]]): F[A] = flatMap(ffa)(identity)

  class MonadOps[A](self: F[A]) extends ApplicativeOps(self) {

    def flatMap[B](f: A => F[B]): F[B] = F.flatMap(self)(f)

    def flatten[B](implicit ev: A <:< F[B]): F[B] = F.flatMap(self)(a => ev(a))

  }

}

object Monad {

  object Implicits {

    implicit def MonadOps[A](self: A)(implicit A: Unify[Monad, A]): Monad[A.F]#MonadOps[A.A] = new A.TC.MonadOps(A.to(self))

  }

}

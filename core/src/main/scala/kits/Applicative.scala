package kits

import scala.language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(a => f(a, _)))

  def map[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ap(fc)(map(fa, fb)((a, b) => f(a, b, _)))

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({ type H[A] = F[G[A]] })#H] =
    new Applicative[({ type H[A] = F[G[A]] })#H] {
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F.map(fga, f)(G.ap(_)(_))
    }

  class ApplicativeOps[A](self: F[A]) extends FunctorOps(self) {

    def ap[B](f: F[A => B]): F[B] = F.ap(self)(f)

    def map[B, C](fb: F[B])(f: (A, B) => C): F[C] = F.map(self, fb)(f)

    def map[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = F.map(self, fb, fc)(f)

  }

}

object Applicative {

  implicit def Ops[A](self: A)(implicit A: Unify[Applicative, A]): Applicative[A.F]#ApplicativeOps[A.A] = new A.TC.ApplicativeOps[A.A](A(self))

  implicit def Monoid[A](implicit A: Monoid[A]): Applicative[({ type F[B] = A })#F] =
    new Applicative[({ type F[B] = A })#F] {
      def pure[B](b: B): A = A.empty
      def ap[B, C](fb: A)(f: A): A = A.append(f, fb)
    }

}

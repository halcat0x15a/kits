package kits

import scala.language.implicitConversions

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map2[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(a => f(a, _)))

  def map3[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ap(fc)(map2(fa, fb)((a, b) => f(a, b, _)))

  def traverse[G[_], A, B](ga: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] = G.traverse(ga)(f)(F)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({ type H[A] = F[G[A]] })#H] =
    new Applicative[({ type H[A] = F[G[A]] })#H] {
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F.map2(fga, f)(G.ap(_)(_))
    }

  class ApplicativeOps[A](self: F[A]) extends FunctorOps(self) {

    def ap[B](f: F[A => B]): F[B] = F.ap(self)(f)

    def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = F.map2(self, fb)(f)

    def map3[B, C, D](fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = F.map3(self, fb, fc)(f)

  }

}

object Applicative {

  object Implicits {

    implicit def ApplicativeOps[A](self: A)(implicit A: Unify[Applicative, A]): Applicative[A.F]#ApplicativeOps[A.A] = new A.TC.ApplicativeOps(A.to(self))

  }

  implicit def Either[E](implicit E: Monoid[E]): Applicative[({ type F[A] = Either[E, A] })#F] =
    new Applicative.EitherApplicative[E] with Functor.EitherFunctor[E] {
      val monoid = E
    }

  trait EitherApplicative[E] extends Applicative[({ type F[A] = Either[E, A] })#F] { self: Functor.EitherFunctor[E] =>

    def monoid: Monoid[E]

    override final def pure[A](a: A): Either[E, A] = Right(a)

    override final def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] =
      (f, fa) match {
        case (Right(f), Right(a)) => Right(f(a))
        case (Left(x), Left(y)) => Left(monoid.append(x, y))
        case (Left(e), _) => Left(e)
        case (_, Left(e)) => Left(e)
      }

  }

}

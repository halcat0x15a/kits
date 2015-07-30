package kits

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(a => f(a, _)))

  def map[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ap(fc)(map(fa, fb)((a, b) => f(a, b, _)))

  def traverse[G[_], A, B](ga: G[A])(f: A => F[B])(implicit G: Traverse[G]): F[G[B]] = G.traverse(ga)(f)(this)

  def compose[G[_]](implicit G: Applicative[G]): Applicative[({ type H[A] = F[G[A]] })#H] =
    new Applicative[({ type H[A] = F[G[A]] })#H] {
      def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
      def ap[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F.ap(fga)(F.map(f)(g => G.ap(_)(g)))
    }

  def dual: Applicative[F] =
    new Applicative[F] {
      def pure[A](a: A): F[A] = F.pure(a)
      def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = F.ap(f)(F.map(fa)(a => f => f(a)))
    }

}

object Applicative {

  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fa)(f)

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  def map[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map(fa, fb)(f)

  def map[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = F.map(fa, fb, fc)(f)

  def map[F[_, _], A, B, X](fa: F[X, A])(f: A => B)(implicit F: Functor[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, B] = F.map(fa)(f)

  def map[F[_, _], A, B, C, X](fa: F[X, A], fb: F[X, B])(f: (A, B) => C)(implicit F: Applicative[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, C] = F.map(fa, fb)(f)

  def map[F[_, _], A, B, C, D, X](fa: F[X, A], fb: F[X, B], fc: F[X, C])(f: (A, B, C) => D)(implicit F: Applicative[({ type G[A] = F[X, A] })#G], dummy: DummyImplicit): F[X, D] = F.map(fa, fb, fc)(f)

  case class Validation[E, A](value: Either[E, A]) extends AnyVal

}

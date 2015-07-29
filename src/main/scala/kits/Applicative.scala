package kits

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))

  def map[A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = ap(fb)(map(fa)(a => f(a, _)))

  def map[A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = ap(fc)(map(fa, fb)((a, b) => f(a, b, _)))

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

  implicit def validation[E](implicit E: Monoid[E]): Applicative[({ type F[A] = Validation[E, A] })#F] =
    new Applicative[({ type F[A] = Validation[E, A] })#F] {
      def pure[A](a: A): Validation[E, A] = Validation(Right(a))
      override def map[A, B](fa: Validation[E, A])(f: A => B): Validation[E, B] = Validation(fa.value.right.map(f))
      def ap[A, B](fa: Validation[E, A])(f: Validation[E, A => B]): Validation[E, B] =
        (f.value, fa.value) match {
          case (Right(f), Right(a)) => Validation(Right(f(a)))
          case (Right(_), Left(e)) => Validation(Left(e))
          case (Left(e), Right(_)) => Validation(Left(e))
          case (Left(x), Left(y)) => Validation(Left(E.append(x, y)))
        }
    }

  case class Validation[E, A](value: Either[E, A]) extends AnyVal

}

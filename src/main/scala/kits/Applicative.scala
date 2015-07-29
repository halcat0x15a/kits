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

  implicit def either[E](implicit E: Monoid[E]): Applicative[({ type F[A] = Either[E, A] })#F] =
    new Applicative[({ type F[A] = Either[E, A] })#F] {
      def pure[A](a: A): Either[E, A] = Right(a)
      override def map[A, B](fa: Either[E, A])(f: A => B): Either[E, B] = fa.right.map(f)
      def ap[A, B](fa: Either[E, A])(f: Either[E, A => B]): Either[E, B] =
        (f, fa) match {
          case (Right(f), Right(a)) => Right(f(a))
          case (Right(_), Left(e)) => Left(e)
          case (Left(e), Right(_)) => Left(e)
          case (Left(x), Left(y)) => Left(E.append(x, y))
        }
    }

}

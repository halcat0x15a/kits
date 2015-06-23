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

  def ap[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F.ap(fa)(f)

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  def map[F[_], A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C)(implicit F: Applicative[F]): F[C] = F.map(fa, fb)(f)

  def map[F[_], A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D)(implicit F: Applicative[F]): F[D] = F.map(fa, fb, fc)(f)

  implicit def right[A](implicit A: Monoid[A]): Applicative[({ type F[B] = Either[A, B] })#F] =
    new Applicative[({ type F[B] = Either[A, B] })#F] {
      def pure[B](b: B): Either[A, B] = Right(b)
      override def map[B, C](fb: Either[A, B])(f: B => C): Either[A, C] = fb.right.map(f)
      def ap[B, C](fb: Either[A, B])(f: Either[A, B => C]): Either[A, C] =
        (f, fb) match {
          case (Right(f), Right(b)) => Right(f(b))
          case (Right(_), Left(a)) => Left(a)
          case (Left(a), Right(_)) => Left(a)
          case (Left(x), Left(y)) => Left(A.append(x, y))
        }
    }

  implicit def left[B](implicit B: Monoid[B]): Applicative[({ type F[A] = Either[A, B] })#F] =
    new Applicative[({ type F[A] = Either[A, B] })#F] {
      def pure[A](a: A): Either[A, B] = Left(a)
      override def map[A, C](fa: Either[A, B])(f: A => C): Either[C, B] = fa.left.map(f)
      def ap[A, C](fa: Either[A, B])(f: Either[A => C, B]): Either[C, B] =
        (f, fa) match {
          case (Left(f), Left(a)) => Left(f(a))
          case (Left(_), Right(b)) => Right(b)
          case (Right(b), Left(_)) => Right(b)
          case (Right(x), Right(y)) => Right(B.append(x, y))
        }
    }

}

package kits

trait Applicative[F[_]] extends Functor[F] { F =>

  def pure[A](a: A): F[A]

  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]

  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))

  def compose[G[_]](implicit G: Applicative[G]) = new Applicative[({ type H[A] = F[G[A]] })#H] {
    def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
    def apply[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F(fga)(F.map(f)(g => G(_)(g)))
  }

  def flip = new Applicative[F] {
    def pure[A](a: A): F[A] = F.pure(a)
    def apply[A, B](fa: F[A])(f: F[A => B]): F[B] = F(f)(F.map(fa)(a => f => f(a)))
  }

}

object Applicative {

  def apply[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F(fa)(f)

  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Functor[F]): F[B] = F.map(fa)(f)

  def map[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(fb)(map(fa)(a => f(a, _)))

  def map[F[_]: Applicative, A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(fc)(map(fa, fb)((a, b) => f(a, b, _)))

  implicit def right[A](implicit A: Monoid[A]) = new Applicative[({ type F[B] = Either[A, B] })#F] {
    def pure[B](b: B): Either[A, B] = Right(b)
    override def map[B, C](fb: Either[A, B])(f: B => C): Either[A, C] = fb.right.map(f)
    def apply[B, C](fb: Either[A, B])(f: Either[A, B => C]): Either[A, C] =
      (f, fb) match {
        case (Right(f), Right(b)) => Right(f(b))
        case (Right(_), Left(a)) => Left(a)
        case (Left(a), Right(_)) => Left(a)
        case (Left(x), Left(y)) => Left(A.append(x, y))
      }
  }

  implicit def left[B](implicit B: Monoid[B]) = new Applicative[({ type F[A] = Either[A, B] })#F] {
    def pure[A](a: A): Either[A, B] = Left(a)
    override def map[A, C](fa: Either[A, B])(f: A => C): Either[C, B] = fa.left.map(f)
    def apply[A, C](fa: Either[A, B])(f: Either[A => C, B]): Either[C, B] =
      (f, fa) match {
        case (Left(f), Left(a)) => Left(f(a))
        case (Left(_), Right(b)) => Right(b)
        case (Right(b), Left(_)) => Right(b)
        case (Right(x), Right(y)) => Right(B.append(x, y))
      }
  }

}

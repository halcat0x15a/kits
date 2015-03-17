package kits

trait Applicative[F[_]] extends Functor[F] { F =>
  def pure[A](a: A): F[A]
  def apply[A, B](fa: F[A])(f: F[A => B]): F[B]
  def map[A, B](fa: F[A])(f: A => B): F[B] = apply(fa)(pure(f))
  def compose[G[_]](implicit G: Applicative[G]) = new Applicative[({ type H[A] = F[G[A]] })#H] {
    def pure[A](a: A): F[G[A]] = F.pure(G.pure(a))
    def apply[A, B](fga: F[G[A]])(f: F[G[A => B]]): F[G[B]] = F(fga)(F.map(f)(g => G(_)(g)))
  }
}

object Applicative {
  def apply[F[_], A, B](fa: F[A])(f: F[A => B])(implicit F: Applicative[F]): F[B] = F(fa)(f)
  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: Applicative[F]): F[B] = F.map(fa)(f)
  def map[F[_]: Applicative, A, B, C](fa: F[A], fb: F[B])(f: (A, B) => C): F[C] = apply(fb)(map(fa)(a => f(a, _)))
  def map[F[_]: Applicative, A, B, C, D](fa: F[A], fb: F[B], fc: F[C])(f: (A, B, C) => D): F[D] = apply(fc)(map(fa, fb)((a, b) => f(a, b, _)))
}

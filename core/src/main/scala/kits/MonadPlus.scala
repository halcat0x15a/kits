package kits

trait MonadPlus[F[_]] extends Monad[F] { F =>

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] =
    flatMap(fa)(a => if (p(a)) pure(a) else zero)

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      lazy val empty: F[A] = zero
      def append(x: F[A], y: F[A]): F[A] = plus(x, y)
    }

  def applicative[A]: Applicative[({ type G[B] = F[A] })#G] =
    new Applicative[({ type G[B] = F[A] })#G] {
      def pure[B](b: B): F[A] = F.zero
      def ap[B, C](fb: F[A])(f: F[A]): F[A] = F.plus(f, fb)
    }

}

object MonadPlus {

  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  def filter[F[_], A](fa: F[A])(p: A => Boolean)(implicit F: MonadPlus[F]): F[A] = F.filter(fa)(p)

  implicit class Ops[F[_], A](val self: F[A])(implicit F: MonadPlus[F]) {

    def withFilter(p: A => Boolean): F[A] = F.filter(self)(p)

  }

}

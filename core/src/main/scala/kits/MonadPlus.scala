package kits

trait MonadPlus[F[_]] extends Monad[F] {

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] =
    flatMap(fa)(a => if (p(a)) pure(a) else zero)

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      lazy val empty: F[A] = zero
      def append(x: F[A], y: F[A]): F[A] = plus(x, y)
    }

}

object MonadPlus {

  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  def filter[F[_], A](fa: F[A])(p: A => Boolean)(implicit F: MonadPlus[F]): F[A] = F.filter(fa)(p)

}

package kits

trait MonadPlus[F[_]] extends Monad[F] { F =>

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] = flatMap(fa)(a => if (p(a)) pure(a) else zero)

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      val empty: F[A] = F.zero[A]
      def append(x: F[A], y: F[A]): F[A] = F.plus(x, y)
    }

}

object MonadPlus {

  def plus[F[_], A](x: F[A], y: F[A])(implicit F: MonadPlus[F]): F[A] = F.plus(x, y)

  def filter[F[_], A](fa: F[A])(p: A => Boolean)(implicit F: MonadPlus[F]): F[A] = F.filter(fa)(p)

}

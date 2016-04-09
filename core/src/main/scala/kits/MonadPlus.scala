package kits

trait MonadPlus[F[_]] extends Monad[F] {

  def zero[A]: F[A]

  def plus[A](x: F[A], y: F[A]): F[A]

  def filter[A](fa: F[A])(p: A => Boolean): F[A] =
    flatMap(fa)(a => if (p(a)) pure(a) else zero)

  def guard(test: Boolean): F[Unit] =
    if (test) pure(()) else zero

}

object MonadPlus {

  def plus[F[_], A](x: F[A], y: F[A])(implicit F: MonadPlus[F]): F[A] = F.plus(x, y)

  def filter[F[_], A](fa: F[A])(p: A => Boolean)(implicit F: MonadPlus[F]): F[A] = F.filter(fa)(p)

  implicit class Ops[F[_], A](val self: F[A])(implicit F: MonadPlus[F]) {

    def withFilter(p: A => Boolean): F[A] = F.filter(self)(p)

  }

}

package kits

trait MonadPlus[F[_]] extends Monad[F] {
  def empty[A]: F[A]
  def plus[A](x: F[A], y: F[A]): F[A]
  def filter[A](fa: F[A])(f: A => Boolean): F[A] =
    flatMap(fa)(a => if (f(a)) pure(a) else empty[A])
  implicit def monoid[A] = new Monoid[F[A]] {
    def zero: F[A] = empty[A]
    def append(x: F[A], y: F[A]): F[A] = plus(x, y)
  }
}

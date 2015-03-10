package kits

trait MonadicPlus[F[_]] extends Monadic[F] {
  def empty[A]: F[A]
  def plus[A](x: F[A], y: F[A]): F[A]
  def filter[A](fa: F[A])(f: A => Boolean): F[A]
}

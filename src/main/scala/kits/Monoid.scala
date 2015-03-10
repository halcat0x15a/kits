package kits

trait Monoid[A] {
  def zero: A
  def append(x: A, y: A): A
}

object Monoid {
  implicit def monoid[F[_], A](implicit F: MonadicPlus[F]) = new Monoid[F[A]] {
    def zero: F[A] = F.empty
    def append(x: F[A], y: F[A]): F[A] = F.plus(x, y)
  }
}

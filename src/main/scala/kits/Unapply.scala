package kits

trait Unapply[T[_[_]], FA] {
  type F[_]
  type A
  val T: T[F]
  def apply(fa: FA): F[A]
}

object Unapply {
  implicit def unapply[T0[_[_]], F0[_], A0](implicit T0: T0[F0]) = new Unapply[T0, F0[A0]] {
    type F[A] = F0[A]
    type A = A0
    val T = T0
    def apply(fa: F[A]) = fa
  }
}

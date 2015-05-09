package kits

trait Instance[T[_[_]], FA] {

  type F[_]

  type A

  val T: T[F]

  def apply(fa: FA): F[A]

}

object Instance extends InstanceImplicits

private[kits] trait InstanceImplicits {

  implicit def identity[T0[_[_]], F0[_], A0](implicit T0: T0[F0]) = new Instance[T0, F0[A0]] {

    type F[A] = F0[A]

    type A = A0

    val T = T0

    def apply(fa: F[A]) = fa

  }

  implicit def constant[T0[_[_]], F0, A0](implicit T0: T0[({ type F[_] = F0 })#F]) = new Instance[T0, F0] {

    type F[_] = F0

    type A = A0

    val T = T0

    def apply(fa: F[A]) = fa

  }

}

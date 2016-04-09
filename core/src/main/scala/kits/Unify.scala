package kits

trait Unify[TC[_[_]], FA] {

  type F[A]

  type A

  val TC: TC[F]

  def apply(fa: FA): F[A]

}

object Unify {

  implicit def FA[TC0[_[_]], F0[_], A0](implicit TC0: TC0[F0]): Unify[TC0, F0[A0]] { type F[A] = F0[A]; type A = A0 } =
    new Unify[TC0, F0[A0]] {
      type F[A] = F0[A]
      type A = A0
      val TC = TC0
      def apply(fa: F[A]): F[A] = fa
    }

  implicit def FAB[TC0[_[_]], F0[_, _], A0, B0](implicit TC0: TC0[({ type F[A] = F0[A0, A] })#F]): Unify[TC0, F0[A0, B0]] { type F[A] = F0[A0, A]; type A = B0 } =
    new Unify[TC0, F0[A0, B0]] {
      type F[A] = F0[A0, A]
      type A = B0
      val TC = TC0
      def apply(fa: F[A]): F[A] = fa
    }

}

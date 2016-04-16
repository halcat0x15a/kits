package kits

trait Unify[TC[_[_]], FA] {

  type F[A]

  type A

  val TC: TC[F]

  def to(fa: FA): F[A]

  def from(fa: F[A]): FA

}

private[kits] abstract class LowPriorityUnifyImplicits {

  implicit def FAB[TC0[_[_]], L, H >: L, F0[_ >: L <: H, _], A0 >: L <: H, B0](implicit TC0: TC0[({ type F[A] = F0[A0, A] })#F]): Unify[TC0, F0[A0, B0]] { type F[A] = F0[A0, A]; type A = B0 } =
    new Unify[TC0, F0[A0, B0]] {
      type F[A] = F0[A0, A]
      type A = B0
      val TC = TC0
      def to(fa: F[A]): F[A] = fa
      def from(fa: F[A]): F[A] = fa
    }

}

object Unify extends LowPriorityUnifyImplicits {

  implicit def FA[TC0[_[_]], F0[_], A0](implicit TC0: TC0[F0]): Unify[TC0, F0[A0]] { type F[A] = F0[A]; type A = A0 } =
    new Unify[TC0, F0[A0]] {
      type F[A] = F0[A]
      type A = A0
      val TC = TC0
      def to(fa: F[A]): F[A] = fa
      def from(fa: F[A]): F[A] = fa
    }

}

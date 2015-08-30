package kits

trait Instance[T[_[_]], FA] {

  type F[A]

  type A

  def T: T[F]

  def apply(fa: FA): F[A]

}

object Instance {

  implicit def fa[T0[_[_]], F0[_], A0](implicit T0: T0[F0]): Instance[T0, F0[A0]] {  type F[A] = F0[A]; type A = A0 } =
    new Instance[T0, F0[A0]] {
      type F[A] = F0[A]
      type A = A0
      def T = T0
      def apply(fa: F[A]): F[A] = fa
    }

  implicit def fab[T0[_[_]], F0[_, _], A0, B0](implicit T0: T0[({ type F[A] = F0[A0, A] })#F]): Instance[T0, F0[A0, B0]] { type F[A] = F0[A0, A]; type A = B0 } =
    new Instance[T0, F0[A0, B0]] {
      type F[A] = F0[A0, A]
      type A = B0
      def T = T0
      def apply(fa: F[A]): F[A] = fa
    }

}

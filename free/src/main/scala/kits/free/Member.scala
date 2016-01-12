package kits.free

trait Member[F[_], U <: Union] {

  def inject[A](fa: F[A]): U { type T = A }

  def project[A](u: U { type T = A }): Option[F[A]]

}

object Member {

  implicit def LeftMember[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](fa: F[A]): (F :+: U) { type T = A } = Inl(fa)
      def project[A](u: (F :+: U) { type T = A }): Option[F[A]] =
        u match {
          case Inl(fa) => Some(fa)
          case Inr(_) => None
        }
    }

  implicit def RightMember[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](fa: F[A]): (G :+: U) { type T = A } = Inr(member.inject(fa))
      def project[A](u: (G :+: U) { type T = A }): Option[F[A]] =
        u match {
          case Inl(_) => None
          case Inr(u) => member.project(u)
        }
    }

}

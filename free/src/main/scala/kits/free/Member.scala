package kits

package free

trait Member[F, U] {

  def inject[A](fa: F { type T = A }): U { type T = A }

  def project[A](u: U { type T = A }): Option[F { type T = A }]

}

object Member {

  def apply[F, U](implicit F: Member[F, U]): Member[F, U] = F

  implicit def LeftMember[F, U]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](fa: F { type T = A }): (F :+: U) { type T = A } = Inl(fa)
      def project[A](u: (F :+: U) { type T = A }): Option[F { type T = A }] =
        u match {
          case Inl(fa) => Some(fa)
          case Inr(_) => None
        }
    }

  implicit def RightMember[F, G, U](implicit F: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](fa: F { type T = A }): (G :+: U) { type T = A } = Inr(F.inject(fa))
      def project[A](u: (G :+: U) { type T = A }): Option[F { type T = A }] =
        u match {
          case Inl(_) => None
          case Inr(u) => F.project(u)
        }
    }

}

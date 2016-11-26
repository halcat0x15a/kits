package kits.free

trait Member[F, U] {

  def inject(fa: F): U

  def project(u: U): Option[F]

}

object Member {

  implicit def LeftMember[F, U]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject(fa: F): F :+: U = Inl(fa)
      def project(u: F :+: U): Option[F] =
        u match {
          case Inl(fa) => Some(fa)
          case Inr(_) => None
        }
    }

  implicit def RightMember[F, G, U](implicit F: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject(fa: F): G :+: U = Inr(F.inject(fa))
      def project(u: G :+: U): Option[F] =
        u match {
          case Inl(_) => None
          case Inr(u) => F.project(u)
        }
    }

}

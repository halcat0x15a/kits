package kits.free

trait Member[F[_], U <: Union] {

  def inject[A](f: F[A]): U

}

object Member {

  implicit def left[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](f: F[A]): F :+: U = Inl(f)
    }

  implicit def right[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](f: F[A]): G :+: U = Inr(member.inject(f))
    }

}

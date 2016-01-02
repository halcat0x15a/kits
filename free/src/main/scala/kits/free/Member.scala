package kits.free

trait Member[F[_], U <: Union] {

  def inject[A](f: F[A]): U { type T = A }

}

object Member {

  implicit def left[F[_], U <: Union]: Member[F, F :+: U] =
    new Member[F, F :+: U] {
      def inject[A](f: F[A]): (F :+: U) { type T = A } = Inl(f)
    }

  implicit def right[F[_], G[_], U <: Union](implicit member: Member[F, U]): Member[F, G :+: U] =
    new Member[F, G :+: U] {
      def inject[A](f: F[A]): (G :+: U) { type T = A } = Inr(member.inject(f))
    }

}

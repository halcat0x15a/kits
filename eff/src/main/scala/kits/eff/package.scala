package kits

package object eff {
  type ~[-A] = A => Nothing

  implicit class LiftableOps[F[_], A](val fa: F[A]) extends AnyVal {
    def lift(implicit F: Liftable[F]): Eff[F.R, A] = F.lift(fa)
  }
}

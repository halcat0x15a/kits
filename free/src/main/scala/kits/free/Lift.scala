package kits

package free

sealed abstract class Lift[M[_]] {

  type T

  type Member[U] = kits.free.Member[Lift[M], U]

}

object Lift {

  case class Apply[M[_], A](value: M[A]) extends Lift[M] { type T = A }

  def apply[U, M[_], A](ma: M[A])(implicit F: Member[Lift[M], U]): Free[U, A] =
    Free(F.inject(Apply(ma)))

  def exec[M[_]](implicit M: Monad[M]) = new Exec {
    type Sum[U] = Lift[M] :+: U
    type F[A] = M[A]
    def exec[A](free: Free[Lift[M] :+: Void, A]): M[A] =
      (free: @unchecked) match {
        case Pure(a) => M.pure(a)
        case Impure(Inl(Apply(ma)), k) => M.flatMap(ma)(a => apply(k(a)))
      }
  }

}

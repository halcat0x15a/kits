package kits

package free

sealed abstract class Lift[M[_]] {

  type T

  type Member[U] = kits.free.Member[Lift[M], U]

}

object Lift {

  case class Wrap[M[_], A](value: M[A]) extends Lift[M] { type T = A }

  def wrap[U, M[_], A](ma: M[A])(implicit F: Member[Lift[M], U]): Free[U, A] =
    Free(F.inject(Wrap(ma)))

  def exec[M[_]](implicit M: Monad[M]) = new Exec {
    type Sum[U] = Lift[M] :+: U
    type F[A] = M[A]
    def exec[A](free: Free[Lift[M] :+: Void, A]): M[A] =
      (free: @unchecked) match {
        case Pure(a) => M.pure(a)
        case Impure(Inl(Wrap(ma)), k) => M.flatMap(ma)(a => exec(k(a)))
      }
  }

}

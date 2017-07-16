package kits

package free

sealed abstract class Lift[M[_]] {

  type Member[U] = kits.free.Member[Lift[M], U]

}

object Lift {

  case class Wrap[M[_], A](value: M[A]) extends Lift[M] { type T = A }

  def run[M[_], A](free: Free[Lift[M] :+: Void, A])(implicit M: Monad[M]): M[A] =
    (free: @unchecked) match {
      case Pure(a) => M.pure(a)
      case Impure(Inl(Wrap(ma)), k) => M.flatMap(ma)(a => run(k(a)))
    }

  def wrap[U, M[_], A](ma: M[A])(implicit F: Member[Lift[M], U]): Free[U, A] =
    Free(F.inject(Wrap(ma)))

  def handle[M[_]](implicit M: Monad[M]) = new Handler {
    type Cons[U] = Lift[M] :+: Void
    type Result[A] = M[A]
    def apply[U, A](free: Free[Lift[M] :+: Void, A]): Free[U, M[A]] = Pure(Lift.run(free))
  }

}

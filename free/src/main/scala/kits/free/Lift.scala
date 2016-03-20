package kits

package free

sealed abstract class Lift[M[_]] {

  type T

  def value: M[T]

  type Member[U <: Union] = kits.free.Member[Lift[M], U]

}

object Lift {

  def apply[U <: Union, M[_], A](ma: M[A])(implicit F: Member[Lift[M], U]): Free[U, A] =
    Free(F.inject(new Lift[M] { type T = A; val value = ma }))

  def run[U <: Union, M[_], A](free: Free[Lift[M] :+: Void, A])(implicit M: Monad[M]): M[A] =
    (free.resume: @unchecked) match {
      case Pure(a) => M.pure(a)
      case Impure(Inl(lift), k) => M.flatMap(lift.value)(a => run(k(a)))
    }

}

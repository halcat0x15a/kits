package kits

package free

import scala.annotation.tailrec

case class Lift[M[_], A](value: M[A])

object Lift {

  def run[U <: Union, M[_], A](free: Free[({ type F[A] = Lift[M, A] })#F :+: Void, A])(implicit M: Monad[M]): M[A] = {
    type F[A] = Lift[M, A]
    (free: Free[F :+: Void, A]) match {
      case Pure(a) => M.pure(a)
      case Impure(Inl(Lift(v)), k) => M.flatMap(v)(a => run(k(a)))
    }
  }

  def lift[U <: Union, M[_], A](value: M[A])(implicit F: Member[({ type F[A] = Lift[M, A] })#F, U]): Free[U, A] = {
    type F[A] = Lift[M, A]
    Free(Lift(value): F[A])
  }

}

package kits

package free

sealed abstract class Choice[M[_]] {

  type T

  type Member[U <: Union] = kits.free.Member[Choice[M], U]

}

object Choice {

  case class Zero[M[_]]() extends Choice[M] { type T = Nothing }

  case class Plus[M[_]]() extends Choice[M] { type T = Boolean }

  def run[U <: Union, M[_], A](free: Free[Choice[M] :+: U, A])(implicit M: MonadPlus[M]): Free[U, M[A]] =
    Free.handleRelay(free, (M.zero[A], Nil: List[Free[Choice[M] :+: U, A]])) {
      case (a, (ma, Nil)) => Right(Pure(M.plus(ma, M.pure(a))): Free[U, M[A]])
      case (a, (ma, x :: xs)) => Left((x, (M.plus(ma, M.pure(a)), xs)))
    } {
      case (Zero(), (ma, Nil)) => _ => Right(Pure(ma))
      case (Zero(), (ma, x :: xs)) => _ => Left((x, (ma, xs)))
      case (Plus(), (ma, stack)) => k => Left((k(true), (ma, k(false) :: stack)))
    }

  def zero[U <: Union, M[_]](implicit F: Member[Choice[M], U]): Free[U, Nothing] = Free(F.inject[Nothing](Zero()))

  def plus[U <: Union, M[_], A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice[M], U]): Free[U, A] = Free[U, Boolean](F.inject(Plus())).flatMap(if (_) x else y)

  def split[U <: Union: Choice[M]#Member, M[_], A](free: Free[U, A]): Free[U, Option[(A, Free[U, A])]] = {
    type F[A] = Free[U, A]
    Free.interpose(free, Nil: List[F[A]])((a, stack) => Right(Pure(Some((a, Traverse.fold(stack)))): Free[U, Option[(A, Free[U, A])]]))((fa: Choice[M], stack) => fa match {
      case Zero() => _ =>
        stack match {
          case Nil => Right(Pure(None))
          case x :: xs => Left((x, xs))
        }
      case Plus() => k => Left((k(true), k(false) :: stack))
    })
  }

}

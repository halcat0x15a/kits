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
    Free.handleRelay(free, (M.zero[A], Nil: List[Free[Choice[M] :+: U, A]]))((a, s) => s match {
      case (ma, Nil) => Right(Pure(M.plus(ma, M.pure(a))): Free[U, M[A]])
      case (ma, x :: xs) => Left((x, (M.plus(ma, M.pure(a)), xs)))
    })((fa, s) => fa match {
      case Zero() => _ =>
        s match {
          case (ma, Nil) => Right(Pure(ma))
          case (ma, x :: xs) => Left((x, (ma, xs)))
        }
      case Plus() => k =>
        val (ma, stack) = s
        Left((k(true), (ma, k(false) :: stack)))
    })

  def zero[U <: Union, M[_]](implicit F: Member[Choice[M], U]): Free[U, Nothing] = Free(F.inject[Nothing](Zero()))

  def plus[U <: Union, M[_], A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice[M], U]): Free[U, A] = Free[U, Boolean](F.inject(Plus())).flatMap(if (_) x else y)

  def split[U <: Union: Choice[M]#Member, M[_], A](free: Free[U, A]): Free[U, Option[(A, Free[U, A])]] = {
    import Traverse.Ops
    Free.interpose(free, Nil: List[Free[U, A]])((a, stack) => Right(Pure(Some((a, stack.foldMap(a => a)))): Free[U, Option[(A, Free[U, A])]]))((fa: Choice[M], stack) => fa match {
      case Zero() => _ =>
        stack match {
          case Nil => Right(Pure(None))
          case x :: xs => Left((x, xs))
        }
      case Plus() => k => Left((k(true), k(false) :: stack))
    })
  }

  def ifte[U <: Union: Choice[M]#Member, M[_], A, B](t: Free[U, A])(th: A => Free[U, B])(el: Free[U, B]): Free[U, B] =
    split(t).flatMap {
      case None => el
      case Some((a, free)) => plus(th(a), free.flatMap(th))
    }

  def once[U <: Union: Choice[M]#Member, M[_], A, B](free: Free[U, A]): Free[U, A] =
    split(free).flatMap {
      case None => zero
      case Some((a, _)) => Pure(a)
    }

}

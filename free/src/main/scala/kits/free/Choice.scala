package kits

package free

sealed abstract class Choice[M[_]] { type T }

object Choice {

  case class Zero[M[_]]() extends Choice[M] { type T = Nothing }

  case class Plus[M[_]]() extends Choice[M] { type T = Boolean }

  def run[U <: Union, M[_], A](free: Free[Choice[M] :+: U, A])(implicit M: MonadPlus[M]): Free[U, M[A]] =
    Free.handleRelay(free)(a => Pure(M.pure(a))) {
      case Zero() => _ => Pure(M.zero)
      case Plus() => k => for (x <- k(true); y <- k(false)) yield M.plus(x, y)
    }

  def zero[U <: Union, M[_]](implicit F: Member[Choice[M], U]): Free[U, Nothing] = Free(F.inject[Nothing](Zero()))

  def plus[U <: Union, M[_], A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice[M], U]): Free[U, A] = Free[U, Boolean](F.inject(Plus())).flatMap(if (_) x else y)

  def reflect[U <: Union, M[_], A](option: Option[(A, Free[U, A])])(implicit F: Member[Choice[M], U]): Free[U, A] =
    option.fold(zero: Free[U, A]) {
      case (a, free) => plus(Pure(a), free)
    }

  def split[U <: Union, M[_], A](free: Free[U, A])(implicit F: Member[Choice[M], U]): Free[U, Option[(A, Free[U, A])]] =
    Free.interpose(free)(a => Pure(Some((a, zero)): Option[(A, Free[U, A])]))((_: Choice[M]) match {
      case Zero() => _ => Pure(None)
      case Plus() => k => for (x <- k(true); y <- k(false)) yield x.fold(y) { case (a, f) => Some((a, plus(f, reflect(y)))) }
    })

}

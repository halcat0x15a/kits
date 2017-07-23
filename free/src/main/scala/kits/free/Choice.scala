package kits

package free

sealed abstract class Choice {

  type Member[U] = kits.free.Member[Choice, U]

}

object Choice {

  case object Zero extends Choice

  case object Plus extends Choice

  def run[U, M[_], A](free: Free[Choice :+: U, A])(implicit M: MonadPlus[M]): Free[U, M[A]] =
    Free.handleRelayS(free, (M.zero[A], List.empty[Free[Choice :+: U, A]]))(
      (a, s) => s match {
        case (ma, Nil) => Right(M.plus(ma, M.pure(a)))
        case (ma, x :: xs) => Left((x, (M.plus(ma, M.pure(a)), xs)))
      },
      (fa, s, k) => (fa, s) match {
        case (Zero, (ma, Nil)) => Right(Pure(ma))
        case (Zero, (ma, x :: xs)) => Left((x, (ma, xs)))
        case (Plus, (ma, stack)) => Left((k(true), (ma, k(false) :: stack)))
      }
    )

  def zero[U, A](implicit F: Member[Choice, U]): Free[U, A] = Free(F.inject(Zero))

  def plus[U, A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice, U]): Free[U, A] = Free[U, Boolean](F.inject(Plus)).flatMap(if (_) x else y)

  def split[U: Choice#Member, A](free: Free[U, A]): Free[U, Option[(A, Free[U, A])]] =
    Free.interposeS(free, List.empty[Free[U, A]])(
      (a, stack) => Right(Some((a, Traverse.fold(stack)))),
      (fa: Choice, stack, k) => fa match {
        case Zero =>
          stack match {
            case Nil => Right(Pure(None))
            case x :: xs => Left((x, xs))
          }
        case Plus => Left((k(true), k(false) :: stack))
      }
    )

  def ifte[U: Choice#Member, A, B](t: Free[U, A])(th: A => Free[U, B])(el: Free[U, B]): Free[U, B] =
    split(t).flatMap {
      case None => el
      case Some((a, free)) => plus(th(a), free.flatMap(th))
    }

  def once[U: Choice#Member, A, B](free: Free[U, A]): Free[U, A] =
    split(free).flatMap {
      case None => zero
      case Some((a, _)) => Pure(a)
    }

  def fromSeq[U: Choice#Member, A](seq: Seq[A]): Free[U, A] = seq.foldRight(zero[U, A])((a, free) => plus(Pure(a), free))

  def handle[M[_]](implicit M: MonadPlus[M]) = new Handler {
    type Cons[U] = Choice :+: U
    type Result[A] = M[A]
    def apply[U, A](free: Free[Choice :+: U, A]): Free[U, M[A]] = Choice.run(free)
  }

}

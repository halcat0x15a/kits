package kits.free

sealed abstract class Choice {

  type T

  type Member[U <: Union] = kits.free.Member[Choice, U]

}

object Choice {

  case object Zero extends Choice { type T = Nothing }

  case object Plus extends Choice { type T = Boolean }

  def run[U <: Union, A](free: Free[Choice :+: U, A]): Free[U, Vector[A]] =
    Free.handleRelay(free)(a => Pure(Vector(a))) {
      case Zero => _ => Pure(Vector.empty)
      case Plus => k => for (x <- k(true); y <- k(false)) yield x ++ y
    }

  def zero[U <: Union](implicit F: Member[Choice, U]): Free[U, Nothing] = Free(F.inject[Nothing](Zero))

  def plus[U <: Union, A](x: Free[U, A], y: Free[U, A])(implicit F: Member[Choice, U]): Free[U, A] = Free[U, Boolean](F.inject(Plus)).flatMap(if (_) x else y)

  def reflect[U <: Union: Choice#Member, A](option: Option[(A, Free[U, A])]): Free[U, A] =
    option.fold(zero: Free[U, A]) {
      case (a, free) => plus(Pure(a), free)
    }

  def split[U <: Union: Choice#Member, A](free: Free[U, A]): Free[U, Option[(A, Free[U, A])]] =
    Free.interpose(free)(a => Pure(Some((a, zero)): Option[(A, Free[U, A])]))((_: Choice) match {
      case Zero => _ => Pure(None)
      case Plus => k => for (x <- k(true); y <- k(false)) yield x.fold(y) { case (a, f) => Some((a, plus(f, reflect(y)))) }
    })

}

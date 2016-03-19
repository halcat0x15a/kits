package kits.free

sealed abstract class Error[E] { type T }

object Error {

  case class Fail[E](value: E) extends Error[E] { type T = Nothing }

  def run[U <: Union, E, A](free: Free[Error[E] :+: U, A]): Free[U, Either[E, A]] =
    Free.fold(free)(a => Pure(Right(a): Either[E, A])) {
      case Fail(e) => _ => Pure(Left(e))
    }

  def fail[U <: Union, E](value: E)(implicit F: Member[Error[E], U]): Free[U, Nothing] = Free(F.inject[Nothing](Fail(value)))

  def recover[U <: Union, E, A](free: Free[U, A])(handle: E => Free[U, A])(implicit F: Member[Error[E], U]): Free[U, A] =
    Free.intercept(free)(a => Pure(a))((_: Error[E]) match {
      case Fail(e) => _ => handle(e)
    })

}

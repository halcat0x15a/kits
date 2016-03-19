package kits.free

sealed abstract class Error[E, +A]

object Error {

  case class Fail[E](value: E) extends Error[E, Nothing]

  def run[U <: Union, E, A, B](free: Free[({ type F[A] = Error[E, A] })#F :+: U, A])(f: A => B)(g: E => B): Free[U, B] = {
    type F[A] = Error[E, A]
    Free.fold(free: Free[F :+: U, A])(a => Pure(f(a))) {
      case Fail(e) => _ => Pure(g(e))
    }
  }

  def toEither[U <: Union, E, A](free: Free[({ type F[A] = Error[E, A] })#F :+: U, A]): Free[U, Either[E, A]] = run(free)(Right(_): Either[E, A])(Left(_))

  def fail[U <: Union, E](value: E)(implicit F: Member[({ type F[A] = Error[E, A] })#F, U]): Free[U, Nothing] = {
    type F[A] = Error[E, A]
    Free(Fail(value): F[Nothing])
  }

  def recover[U <: Union, E, A](free: Free[U, A])(handle: E => Free[U, A])(implicit F: Member[({ type F[A] = Error[E, A] })#F, U]): Free[U, A] = {
    type F[A] = Error[E, A]
    Free.intercept(free)(a => Pure(a)) { (fa: F[Any]) =>
      fa match {
        case Fail(e) => _ => handle(e)
      }
    }
  }

}

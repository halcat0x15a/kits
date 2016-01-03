package kits.free

import scala.annotation.tailrec

sealed abstract class Error[E, +A]

object Error {

  case class Fail[E](value: E) extends Error[E, Nothing]

  def run[U <: Union, E, A](free: Free[({ type F[A] = Error[E, A] })#F :+: U, A]): Free[U, Either[E, A]] = {
    type F[A] = Error[E, A]
    (free: Free[F :+: U, A]) match {
      case Pure(a) => Pure(Right(a))
      case Impure(Inl(Fail(e)), k) => Pure(Left(e))
      case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x))))
    }
  }

  def fail[U <: Union, E](value: E)(implicit F: Member[({ type F[A] = Error[E, A] })#F, U]): Free[U, Nothing] = {
    type F[A] = Error[E, A]
    Free(Fail(value): F[Nothing])
  }

  def recover[U <: Union, E, A](free: Free[U, A])(handle: E => Free[U, A])(implicit F: Member[({ type F[A] = Error[E, A] })#F, U]): Free[U, A] = {
    @tailrec
    def loop(free: Free[U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Fail(e)) => loop(handle(e))
            case None => Impure(u, Arrows.singleton((x: Any) => recover(k(x))(handle)))
          }
      }
    loop(free)
  }

}

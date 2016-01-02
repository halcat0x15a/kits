package kits.free

import scala.annotation.tailrec

sealed abstract class Error[T, +A]

object Error {

  case class Fail[T](value: T) extends Error[T, Nothing]

  def run[U <: Union, T, A](free: Free[({ type F[A] = Error[T, A] })#F :+: U, A]): Free[U, Either[T, A]] = {
    type F[A] = Error[T, A]
    def go(free: Free[F :+: U, A]): Free[U, Either[T, A]] =
      free match {
        case Pure(a) => Pure(Right(a))
        case Impure(Inl(Fail(e)), k) => Pure(Left(e))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x))))
      }
    go(free)
  }

  def fail[U <: Union, T](value: T)(implicit F: Member[({ type F[A] = Error[T, A] })#F, U]): Free[U, Nothing] = {
    type F[A] = Error[T, A]
    Free(Fail(value): F[Nothing])
  }

  def recover[U <: Union, T, A](free: Free[U, A])(handle: T => Free[U, A])(implicit F: Member[({ type F[A] = Error[T, A] })#F, U]): Free[U, A] = {
    @tailrec
    def go(free: Free[U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Fail(e)) => go(handle(e))
            case None => Impure(u, Arrows.singleton((x: Any) => recover(k(x))(handle)))
          }
      }
    go(free)
  }

}

package kits.free

import scala.annotation.tailrec

sealed abstract class Reader[T, +A]

case class Get[T]() extends Reader[T, T]

object Reader {

  def run[U <: Union, T, A](free: Free[({ type F[A] = Reader[T, A] })#F :+: U, A], value: T): Free[U, A] = {
    type F[A] = Reader[T, A]
    @tailrec
    def go(free: Free[F :+: U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Get()), k) => go(k(value))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x), value)))
      }
    go(free)
  }

  def ask[U <: Union, T](implicit F: Member[({ type F[A] = Reader[T, A] })#F, U]): Free[U, T] = {
    type F[A] = Reader[T, A]
    Free(Get(): F[T])
  }

  def local[U <: Union, T, A](free: Free[U, A])(f: T => T)(implicit F: Member[({ type F[A] = Reader[T, A] })#F, U]): Free[U, A] = {
    @tailrec
    def go(free: Free[U, A], value: T): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Get()) => go(k(value), value)
            case None => Impure(u, Arrows.singleton((x: Any) => local(k(x))(f)))
          }
      }
    ask.flatMap(a => go(free, f(a)))
  }

}

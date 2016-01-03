package kits.free

import scala.annotation.tailrec

sealed abstract class Reader[T, +A]

object Reader {

  case class Ask[T]() extends Reader[T, T]

  def run[U <: Union, T, A](free: Free[({ type F[A] = Reader[T, A] })#F :+: U, A], value: T): Free[U, A] = {
    type F[A] = Reader[T, A]
    @tailrec
    def loop(free: Free[F :+: U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Ask()), k) => loop(k(value))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x), value)))
      }
    loop(free)
  }

  def ask[U <: Union, T](implicit F: Member[({ type F[A] = Reader[T, A] })#F, U]): Free[U, T] = {
    type F[A] = Reader[T, A]
    Free(Ask(): F[T])
  }

  def local[U <: Union, T, A](free: Free[U, A])(f: T => T)(implicit F: Member[({ type F[A] = Reader[T, A] })#F, U]): Free[U, A] = {
    def go(free: Free[U, A], value: T): Free[U, A] = loop(free, value)
    @tailrec
    def loop(free: Free[U, A], value: T): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Ask()) => loop(k(value), value)
            case None => Impure(u, Arrows.singleton((x: Any) => go(k(x), value)))
          }
      }
    ask.flatMap(a => loop(free, f(a)))
  }

}

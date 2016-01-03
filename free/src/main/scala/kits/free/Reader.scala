package kits.free

import scala.annotation.tailrec

sealed abstract class Reader[R, +A]

object Reader {

  case class Ask[R]() extends Reader[R, R]

  def run[U <: Union, R, A](free: Free[({ type F[A] = Reader[R, A] })#F :+: U, A], value: R): Free[U, A] = {
    type F[A] = Reader[R, A]
    @tailrec
    def loop(free: Free[F :+: U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Ask()), k) => loop(k(value))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x), value)))
      }
    loop(free)
  }

  def ask[U <: Union, R](implicit F: Member[({ type F[A] = Reader[R, A] })#F, U]): Free[U, R] = {
    type F[A] = Reader[R, A]
    Free(Ask(): F[R])
  }

  def local[U <: Union, R, A](free: Free[U, A])(f: R => R)(implicit F: Member[({ type F[A] = Reader[R, A] })#F, U]): Free[U, A] = {
    def go(free: Free[U, A], value: R): Free[U, A] = loop(free, value)
    @tailrec
    def loop(free: Free[U, A], value: R): Free[U, A] =
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

package kits.free

import scala.annotation.tailrec

case class Trampoline[A]()

object Trampoline {

  def apply[U <: Union, A](free: => Free[Trampoline :+: U, A]): Free[Trampoline :+: U, A] =
    Impure(Inl(Trampoline()), Leaf((_: Any) => free))

  def run[U <: Union, A](free: Free[Trampoline :+: U, A]): Free[U, A] = {
    @tailrec
    def go(free: Free[Trampoline :+: U, A]): Free[U, A] =
      free match {
        case Pure(v) => Pure(v)
        case impure@Impure() =>
          impure.union match {
            case inl@Inl() => go(impure.arrows(().asInstanceOf[impure.T]))
            case Inr(u) => Impure(u, Leaf((x: impure.T) => run(impure.arrows(x))))
          }
      }
    go(free)
  }

}

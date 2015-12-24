package kits.free

sealed abstract class Reader[A]

case class Get[A]() extends Reader[A]

object Reader {

  def run[U <: Union, A](free: Free[Reader :+: U, A], a: A): Free[U, A] =
    Free.fold(free)(a => Pure(a)) { case (Get(), k) => k(a) }

  def ask[U <: Union, A](implicit member: Member[Reader, U]): Free[U, A] =
    Impure(member.inject(Get()), Leaf((x: A) => Pure(x)))

}

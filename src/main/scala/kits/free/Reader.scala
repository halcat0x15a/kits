package kits.free

sealed abstract class Reader[T, +A]

case class Get[T]() extends Reader[T, T]

object Reader {

  def run[U <: Union, T, A](free: Free[({ type F[A] = Reader[T, A] })#F :+: U, A], value: T): Free[U, A] = {
    type F[A] = Reader[T, A]
    @scala.annotation.tailrec
    def go(free: Free[F :+: U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        case Impure(Inl(Get()), f) => go(f(value))
        case Impure(Inr(u), f) => Impure(u, Arrows.singleton((x: Any) => run(f(x), value)))
      }
    go(free)
  }

  def ask[U <: Union, T](implicit member: Member[({ type F[A] = Reader[T, A] })#F, U]): Free[U, T] = {
    type F[A] = Reader[T, A]
    Free(Get(): F[T])
  }

}

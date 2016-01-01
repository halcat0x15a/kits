package kits.free

sealed abstract class Writer[T, +A]

case class Put[T](value: T) extends Writer[T, Unit]

object Writer {

  def run[U <: Union, T, A](free: Free[({ type F[A] = Writer[T, A] })#F :+: U, A]): Free[U, (A, Vector[T])] = {
    type F[A] = Writer[T, A]
    @scala.annotation.tailrec
    def go(free: Free[F :+: U, A], acc: Vector[T]): Free[U, (A, Vector[T])] =
      free match {
        case Pure(a) => Pure((a, acc))
        case Impure(Inl(Put(o)), f) => go(f(()), acc :+ o)
        case Impure(Inr(u), f) => Impure(u, Arrows.singleton((x: Any) => run(f(x))))
      }
    go(free, Vector.empty)
  }

  def tell[U <: Union, T](value: T)(implicit member: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, Unit] = {
    type F[A] = Writer[T, A]
    Free(Put(value): F[Unit])
  }

}

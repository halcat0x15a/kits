package kits.free

trait Writer[O] {

  sealed abstract class Writer[+A]

  case class Put(value: O) extends Writer[Unit]

  def run[U <: Union, A](free: Free[Writer :+: U, A]): Free[U, (A, Vector[O])] = {
    @scala.annotation.tailrec
    def go(free: Free[Writer :+: U, A], acc: Vector[O]): Free[U, (A, Vector[O])] =
      free match {
        case Pure(a) => Pure((a, acc))
        case Impure(Inl(Put(o)), f) => go(f(()), acc :+ o)
        case Impure(Inr(u), f) => Impure(u, Leaf((x: Any) => run(f(x))))
      }
    go(free, Vector.empty)
  }

  def tell[U <: Union](value: O)(implicit member: Member[Writer, U]): Free[U, Unit] = Free(Put(value))

}

package kits.free

trait Writer[O] {

  sealed abstract class Writer[A]

  case class Put(value: O) extends Writer[Unit]

  def run[U <: Union, A](free: Free[Writer :+: U, A]): Free[U, (A, Vector[O])] = {
    //@scala.annotation.tailrec
    def go(free: Free[Writer :+: U, A], acc: Vector[O]): Free[U, (A, Vector[O])] =
      free match {
        case Pure(a) => Pure((a, acc))
        case ImpureL(Put(o), f) => go(f(()), acc :+ o)
        case ImpureR(u, f) => Impure(u, Leaf((x: A) => run(f(x))))
      }
    go(free, Vector.empty)
  }

  def tell[U <: Union](value: O)(implicit member: Member[Writer, U]): Free[U, Unit] =
    Impure(member.inject(Put(value)), Leaf(Pure(_: Unit)))

}

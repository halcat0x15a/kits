package kits.free

sealed abstract class Writer[A]

case class Put(value: String) extends Writer[Unit]

object Writer {

  def run[U <: Union, A](free: Free[Writer :+: U, A]): Free[U, (A, List[String])] =
    Free.fold(free)(a => Pure((a, List.empty[String]))) {
      case (Put(w), k) => k(()).map { case (a, l) => (a, w :: l) }
    }

  def tell[U <: Union](value: String)(implicit member: Member[Writer, U]): Free[U, Unit] =
    Impure(member.inject(Put(value)), Leaf(Pure(_: Unit)))

}

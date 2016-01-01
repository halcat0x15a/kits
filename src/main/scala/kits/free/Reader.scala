package kits.free

trait Reader[I] {

  sealed abstract class Reader[A]

  case class Get() extends Reader[I]

  def run[U <: Union, A](free: Free[Reader :+: U, A], i: I): Free[U, A] = {
    @scala.annotation.tailrec
    def go(free: Free[Reader :+: U, A]): Free[U, A] =
      free match {
        case Pure(a) => Pure(a)
        //case ImpureL(Get(), f) => ???
        case impure@Impure() =>
          impure.union match {
            case Inl(_) => go(impure.arrows(i.asInstanceOf[impure.T]))
            case Inr(u) => Impure(u, Leaf((x: impure.T) => run(impure.arrows(x), i)))
          }
      }
    go(free)
  }

  def ask[U <: Union](implicit member: Member[Reader, U]): Free[U, I] =
    Impure(member.inject(Get()), Leaf((x: I) => Pure(x)))

}

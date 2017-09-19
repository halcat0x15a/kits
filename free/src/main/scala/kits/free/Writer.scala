package kits

package free

sealed abstract class Writer[W] {

  type Member[U] = kits.free.Member[Writer[W], U]

}

object Writer {

  case class Tell[W](value: W) extends Writer[W]

  def run[U, W, A](free: Free[Writer[W] :+: U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.handleRelayS(free, W.empty)(
      (a, w) => Right((w, a)),
      (fa, w, k) => fa match {
        case Tell(v) => Left((k(()), W.append(w, v)))
      }
    )

  def runVec[U, W, A](free: Free[Writer[W] :+: U, A]): Free[U, (Vector[W], A)] =
    Free.handleRelayS(free, Vector.empty[W])(
      (a, w) => Right((w, a)),
      (fa, w, k) => fa match {
        case Tell(v) => Left((k(()), w :+ v))
      }
    )

  def tell[W](value: W) = new Eff[Unit] {
    type Effects = EffLeaf[Writer[W]]
    def free[U](implicit members: EffMember[Effects, U]): Free[U, Unit] = Free(members.get.inject(Tell(value)))
  }

  def listen[Effs <: EffTree, W, A](eff: Eff[A] { type Effects = Effs })(implicit W: Monoid[W], get: GetMember[Effs, Writer[W]]) =
    new Eff[(W, A)] {
      type Effects = Effs
      def free[U](implicit members: EffMember[Effects, U]): Free[U, (W, A)] =
        Free.interposeS(eff.free[U], W.empty)(
          (a, w) => Right((w, a)),
          (fa: Writer[W], w, k) => fa match {
            case Tell(v) => Left((k(()), W.append(w, v)))
          }
        )(get(members))
    }.flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }

  def handle[W](implicit W: Monoid[W]) = new Handler {
    type Cons[U] = Writer[W] :+: U
    type Result[A] = (W, A)
    def apply[U, A](free: Free[Writer[W] :+: U, A]): Free[U, (W, A)] = Writer.run(free)
  }

  def handleVec[W] = new Handler {
    type Cons[U] = Writer[W] :+: U
    type Result[A] = (Vector[W], A)
    def apply[U, A](free: Free[Writer[W] :+: U, A]): Free[U, (Vector[W], A)] = Writer.runVec(free)
  }

}

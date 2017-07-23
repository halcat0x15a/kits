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

  def tell[U, W](value: W)(implicit F: Member[Writer[W], U]): Free[U, Unit] = Free(F.inject(Tell(value)))

  def listen[U: Writer[W]#Member, W, A](free: Free[U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.interposeS(free, W.empty)(
      (a, w) => Right((w, a)),
      (fa: Writer[W], w, k) => fa match {
        case Tell(v) => Left((k(()), W.append(w, v)))
      }
    ).flatMap {
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

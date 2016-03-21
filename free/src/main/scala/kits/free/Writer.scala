package kits

package free

sealed abstract class Writer[W] {

  type T

  type Member[U <: Union] = kits.free.Member[Writer[W], U]

}

object Writer {

  case class Tell[W](value: W) extends Writer[W] { type T = Unit }

  def run[U <: Union, W, A](free: Free[Writer[W] :+: U, A]): Free[U, (Vector[W], A)] =
    Free.handleRelay(free)(a => Pure((Vector.empty[W], a))) {
      case Tell(v) => k => k(()).map { case (w, a) => (v +: w, a) }
    }

  def tell[U <: Union, W](value: W)(implicit F: Member[Writer[W], U]): Free[U, Unit] = Free(F.inject(Tell(value)))

  def listen[U <: Union: Writer[W]#Member, W, A](free: Free[U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.interpose(free)(a => Pure((W.empty, a)))((_: Writer[W]) match {
      case Tell(v) => k => k(()).map { case (w, a) => (W.append(v, w), a) }
    }).flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }

  def pass[U <: Union: Writer[W]#Member, W: Monoid, A](free: Free[U, (W => W, A)]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

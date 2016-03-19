package kits

package free

sealed abstract class Writer[W] { type T }

object Writer {

  case class Tell[W](value: W) extends Writer[W] { type T = Unit }

  def run[U <: Union, W, A](free: Free[Writer[W] :+: U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.fold(free)(a => Pure((W.empty, a))) {
      case Tell(v) => k => k(()).map { case (w, a) => (W.append(v, w), a) }
    }

  def tell[U <: Union, W](value: W)(implicit F: Member[Writer[W], U]): Free[U, Unit] = Free(F.inject(Tell(value)))

  def listen[U <: Union, W, A](free: Free[U, A])(implicit F: Member[Writer[W], U], W: Monoid[W]): Free[U, (W, A)] =
    Free.intercept(free)(a => Pure((W.empty, a)))((_: Writer[W]) match {
      case Tell(v) => k => k(()).map { case (w, a) => (W.append(v, w), a) }
    }) flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }

  def pass[U <: Union, W: Monoid, A](free: Free[U, (W => W, A)])(implicit F: Member[Writer[W], U]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

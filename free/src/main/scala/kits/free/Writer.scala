package kits

package free

sealed abstract class Writer[W, +A]

object Writer {

  case class Tell[W](value: W) extends Writer[W, Unit]

  def run[U <: Union, W, A](free: Free[({ type F[A] = Writer[W, A] })#F :+: U, A])(implicit W: Monoid[W]): Free[U, (W, A)] = {
    type F[A] = Writer[W, A]
    Free.fold(free: Free[F :+: U, A])(a => Pure((W.empty, a))) {
      case Tell(v) => k => k(()).map { case (w, a) => (W.append(v, w), a) }
    }
  }

  def tell[U <: Union, W](value: W)(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U]): Free[U, Unit] = {
    type F[A] = Writer[W, A]
    Free(Tell(value): F[Unit])
  }

  def listen[U <: Union, W, A](free: Free[U, A])(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U], W: Monoid[W]): Free[U, (W, A)] = {
    type F[A] = Writer[W, A]
    Free.intercept(free)(a => Pure((W.empty, a))) { (fa: F[Any]) =>
      fa match {
        case Tell(v) => k => k(()).map { case (w, a) => (W.append(v, w), a) }
      }
    } flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }
  }

  def pass[U <: Union, W: Monoid, A](free: Free[U, (W => W, A)])(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

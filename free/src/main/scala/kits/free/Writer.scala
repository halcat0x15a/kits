package kits

package free

sealed abstract class Writer[W] {

  type T

  type Member[U <: Union] = kits.free.Member[Writer[W], U]

}

object Writer {

  case class Tell[W](value: W) extends Writer[W] { type T = Unit }

  def run[U <: Union, W, A](free: Free[Writer[W] :+: U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.handleRelay(free, W.empty)((a, w) => Right(Pure((w, a)): Free[U, (W, A)]))((fa, w) => fa match {
      case Tell(v) => k => Left((k(()), W.append(w, v)))
    })
  
  def exec[U <: Union, W: Monoid, A](free: Free[Writer[W] :+: U, A]): Free[U, W] = run(free).map(_._1)

  def tell[U <: Union, W](value: W)(implicit F: Member[Writer[W], U]): Free[U, Unit] = Free(F.inject(Tell(value)))

  def listen[U <: Union: Writer[W]#Member, W, A](free: Free[U, A])(implicit W: Monoid[W]): Free[U, (W, A)] =
    Free.interpose(free, W.empty)((a, w) => Right(Pure((w, a)): Free[U, (W, A)]))((fa: Writer[W], w) => fa match {
      case Tell(v) => k => Left((k(()), W.append(w, v)))
    }).flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }

  def pass[U <: Union: Writer[W]#Member, W: Monoid, A](free: Free[U, (W => W, A)]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

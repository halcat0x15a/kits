package kits

package free

import scala.annotation.tailrec

sealed abstract class Writer[W, +A]

object Writer {

  case class Tell[W](value: W) extends Writer[W, Unit]

  def run[U <: Union, W, A](free: Free[({ type F[A] = Writer[W, A] })#F :+: U, A])(implicit W: Monoid[W]): Free[U, (W, A)] = {
    type F[A] = Writer[W, A]
    def go(free: Free[F :+: U, A], acc: W): Free[U, (W, A)] = loop(free, acc)
    @tailrec
    def loop(free: Free[F :+: U, A], acc: W): Free[U, (W, A)] = {
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(Inl(Tell(v)), k) => loop(k(()), W.append(acc, v))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => go(k(x), acc)))
      }
    }
    loop(free, W.empty)
  }

  def tell[U <: Union, W](value: W)(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U]): Free[U, Unit] = {
    type F[A] = Writer[W, A]
    Free(Tell(value): F[Unit])
  }

  def listen[U <: Union, W, A](free: Free[U, A])(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U], W: Monoid[W]): Free[U, (W, A)] = {
    def go(free: Free[U, A], acc: W): Free[U, (W, A)] = loop(free, acc)
    @tailrec
    def loop(free: Free[U, A], acc: W): Free[U, (W, A)] =
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Tell(v)) => loop(k(()), W.append(acc, v))
            case None => Impure(u, Arrows.singleton((x: Any) => go(k(x), acc)))
          }
      }
    loop(free, W.empty).flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }
  }

  def pass[U <: Union, W: Monoid, A](free: Free[U, (W => W, A)])(implicit F: Member[({ type F[A] = Writer[W, A] })#F, U]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

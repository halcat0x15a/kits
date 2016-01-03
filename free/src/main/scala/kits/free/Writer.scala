package kits

package free

import scala.annotation.tailrec

sealed abstract class Writer[T, +A]

object Writer {

  case class Tell[T](value: T) extends Writer[T, Unit]

  def run[U <: Union, T, A](free: Free[({ type F[A] = Writer[T, A] })#F :+: U, A])(implicit T: Monoid[T]): Free[U, (T, A)] = {
    type F[A] = Writer[T, A]
    def go(free: Free[F :+: U, A], acc: T): Free[U, (T, A)] = loop(free, acc)
    @tailrec
    def loop(free: Free[F :+: U, A], acc: T): Free[U, (T, A)] = {
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(Inl(Tell(v)), k) => loop(k(()), T.append(acc, v))
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => go(k(x), acc)))
      }
    }
    loop(free, T.empty)
  }

  def tell[U <: Union, T](value: T)(implicit F: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, Unit] = {
    type F[A] = Writer[T, A]
    Free(Tell(value): F[Unit])
  }

  def listen[U <: Union, T, A](free: Free[U, A])(implicit F: Member[({ type F[A] = Writer[T, A] })#F, U], T: Monoid[T]): Free[U, (T, A)] = {
    def go(free: Free[U, A], acc: T): Free[U, (T, A)] = loop(free, acc)
    @tailrec
    def loop(free: Free[U, A], acc: T): Free[U, (T, A)] =
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(u, k) =>
          F.project(u) match {
            case Some(Tell(v)) => loop(k(()), T.append(acc, v))
            case None => Impure(u, Arrows.singleton((x: Any) => go(k(x), acc)))
          }
      }
    loop(free, T.empty).flatMap {
      case r@(w, _) => tell(w).map(_ => r)
    }
  }

  def pass[U <: Union, T: Monoid, A](free: Free[U, (T => T, A)])(implicit F: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, A] =
    listen(free).flatMap {
      case (w, (f, a)) => tell(f(w)).map(_ => a)
    }

}

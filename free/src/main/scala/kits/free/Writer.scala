package kits

package free

import scala.annotation.tailrec

sealed abstract class Writer[T, +A]

case class Put[T](value: T) extends Writer[T, Unit]

object Writer {

  def run[U <: Union, T, A](free: Free[({ type F[A] = Writer[T, A] })#F :+: U, A]): Free[U, (Vector[T], A)] = {
    type F[A] = Writer[T, A]
    @tailrec
    def go(free: Free[F :+: U, A], acc: Vector[T]): Free[U, (Vector[T], A)] =
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(Inl(Put(v)), k) => go(k(()), acc :+ v)
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x))))
      }
    go(free, Vector.empty)
  }

  def tell[U <: Union, T](value: T)(implicit member: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, Unit] = {
    type F[A] = Writer[T, A]
    Free(Put(value): F[Unit])
  }

  def listen[U <: Union, T, A](free: Free[U, A])(implicit member: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, (Vector[T], A)] = {
    @tailrec
    def go(free: Free[U, A], acc: Vector[T]): Free[U, (Vector[T], A)] =
      free match {
        case Pure(a) => Pure((acc, a))
        case Impure(u, k) =>
          member.project(u) match {
            case Some(Put(v)) => go(k(()), acc :+ v)
            case None => Impure(u, Arrows.singleton((x: Any) => listen(k(x))))
          }
      }
    go(free, Vector.empty).flatMap {
      case r@(ws, _) =>
        type F[A] = Free[U, A]
        Traverse.traverse(ws)(w => tell(w): F[Unit]).map(_ => r)
    }
  }

  def pass[U <: Union, T, A](free: Free[U, (T => T, A)])(implicit member: Member[({ type F[A] = Writer[T, A] })#F, U]): Free[U, A] = {
    listen(free).flatMap {
      case (ws, (f, a)) =>
        type F[A] = Free[U, A]
        Traverse.traverse(ws)(w => tell(f(w)): F[Unit]).map(_ => a)
    }
  }

}

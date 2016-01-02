package kits

package free

import scala.annotation.tailrec

sealed abstract class State[T, +A]

object State {

  case class Get[T]() extends State[T, T]

  case class Put[T](value: T) extends State[T, Unit]

  def run[U <: Union, T, A](free: Free[({ type F[A] = State[T, A] })#F :+: U, A], state: T): Free[U, (T, A)] = {
    type F[A] = State[T, A]
    @tailrec
    def go(free: Free[F :+: U, A], state: T): Free[U, (T, A)] =
      free match {
        case Pure(a) => Pure((state, a))
        case Impure(Inl(Get()), k) => go(k(state), state)
        case Impure(Inl(Put(v)), k) => go(k(()), v)
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x), state)))
      }
    go(free, state)
  }

  def get[U <: Union, T](implicit F: Member[({ type F[A] = State[T, A] })#F, U]): Free[U, T] = {
    type F[A] = State[T, A]
    Free(Get(): F[T])
  }

  def put[U <: Union, T](value: T)(implicit F: Member[({ type F[A] = State[T, A] })#F, U]): Free[U, Unit] = {
    type F[A] = State[T, A]
    Free(Put(value): F[Unit])
  }

  def modify[U <: Union, T](f: T => T)(implicit F: Member[({ type F[A] = State[T, A] })#F, U]): Free[U, Unit] =
    for {
      a <- get
      _ <- put(f(a))
    } yield ()

}

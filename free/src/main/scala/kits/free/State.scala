package kits

package free

import scala.annotation.tailrec

sealed abstract class State[S, +A]

object State {

  case class Get[S]() extends State[S, S]

  case class Put[S](state: S) extends State[S, Unit]

  def run[U <: Union, S, A](free: Free[({ type F[A] = State[S, A] })#F :+: U, A], state: S): Free[U, (S, A)] = {
    type F[A] = State[S, A]
    @tailrec
    def loop(free: Free[F :+: U, A], state: S): Free[U, (S, A)] =
      free match {
        case Pure(a) => Pure((state, a))
        case Impure(Inl(Get()), k) => loop(k(state), state)
        case Impure(Inl(Put(v)), k) => loop(k(()), v)
        case Impure(Inr(u), k) => Impure(u, Arrows.singleton((x: Any) => run(k(x), state)))
      }
    loop(free, state)
  }

  def eval[U <: Union, S, A](free: Free[({ type F[A] = State[S, A] })#F :+: U, A], state: S): Free[U, A] = run(free, state).map(_._2)

  def exec[U <: Union, S, A](free: Free[({ type F[A] = State[S, A] })#F :+: U, A], state: S): Free[U, S] = run(free, state).map(_._1)

  def get[U <: Union, S](implicit F: Member[({ type F[A] = State[S, A] })#F, U]): Free[U, S] = {
    type F[A] = State[S, A]
    Free(Get(): F[S])
  }

  def put[U <: Union, S](state: S)(implicit F: Member[({ type F[A] = State[S, A] })#F, U]): Free[U, Unit] = {
    type F[A] = State[S, A]
    Free(Put(state): F[Unit])
  }

  def modify[U <: Union, S](f: S => S)(implicit F: Member[({ type F[A] = State[S, A] })#F, U]): Free[U, Unit] =
    for {
      a <- get
      _ <- put(f(a))
    } yield ()

}

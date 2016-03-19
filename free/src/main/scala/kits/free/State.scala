package kits

package free

sealed abstract class State[S, +A]

object State {

  case class Get[S]() extends State[S, S]

  case class Put[S](state: S) extends State[S, Unit]

  def run[U <: Union, S, A](free: Free[({ type F[A] = State[S, A] })#F :+: U, A], state: S): Free[U, (S, A)] = {
    type F[A] = State[S, A]
    Free.accum(free: Free[F :+: U, A], state)((a, s) => Pure((s, a))) {
      case Get() => s => k => k(s, s)
      case Put(s) => _ => k => k((), s)
    }
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

  def modify[U <: Union, S](f: S => S)(implicit F: Member[({ type F[A] = State[S, A] })#F, U]): Free[U, Unit] = get.flatMap(a => put(f(a)))

}

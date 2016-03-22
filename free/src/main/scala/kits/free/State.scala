package kits.free

sealed abstract class State[S] {

  type T

  type Member[U <: Union] = kits.free.Member[State[S], U]

}

object State {

  case class Get[S]() extends State[S] { type T = S }

  case class Put[S](state: S) extends State[S] { type T = Unit }

  def run[U <: Union, S, A](free: Free[State[S] :+: U, A], state: S): Free[U, (S, A)] =
    Free.handleRelay(free, state)((a, s) => Right(Pure((s, a)): Free[U, (S, A)]))((fa, s) => fa match {
      case Get() => k => Left((k(s), s))
      case Put(s) => k => Left((k(()), s))
    })

  def eval[U <: Union, S, A](free: Free[State[S] :+: U, A], state: S): Free[U, A] = run(free, state).map(_._2)

  def exec[U <: Union, S, A](free: Free[State[S] :+: U, A], state: S): Free[U, S] = run(free, state).map(_._1)

  def get[U <: Union, S](implicit F: Member[State[S], U]): Free[U, S] = Free(F.inject(Get()))

  def put[U <: Union, S](state: S)(implicit F: Member[State[S], U]): Free[U, Unit] = Free(F.inject(Put(state)))

  def modify[U <: Union: State[S]#Member, S](f: S => S): Free[U, Unit] = get.flatMap(a => put(f(a)))

}

package kits.free

sealed abstract class State[S] {

  type Member[U] = kits.free.Member[State[S], U]

}

object State {

  case class Get[S]() extends State[S]

  case class Put[S](state: S) extends State[S]

  def run[U, S, A](free: Free[State[S] :+: U, A], state: S): Free[U, (S, A)] =
    Free.handleRelay(free, state)((a, s) => Right((s, a))) {
      case (Get(), s) => k => Left((k(s), s))
      case (Put(s), _) => k => Left((k(()), s))
    }

  def get[U, S](implicit F: Member[State[S], U]): Free[U, S] = Free(F.inject(Get()))

  def put[U, S](state: S)(implicit F: Member[State[S], U]): Free[U, Unit] = Free(F.inject(Put(state)))

  def modify[U: State[S]#Member, S](f: S => S): Free[U, Unit] = get.flatMap(a => put(f(a)))

  def handle[S](state: S) = new Handler {
    type Cons[U] = State[S] :+: U
    type Result[A] = (S, A)
    def apply[U, A](free: Free[State[S] :+: U, A]): Free[U, (S, A)] = State.run(free, state)
  }

}

package kits.free

sealed abstract class State[S] {

  type Member[U] = kits.free.Member[State[S], U]

}

object State {

  case class Get[S]() extends State[S]

  case class Put[S](state: S) extends State[S]

  def run[S](state: S) = new Run {
    type Sum[U] = State[S] :+: U
    type F[A] = (S, A)
    def run[U, A](free: Free[State[S] :+: U, A]): Free[U, (S, A)] =
      Free.handleRelay(free, state)((a, s) => Right((s, a))) {
        case (Get(), s) => k => Left((k(s), s))
        case (Put(s), _) => k => Left((k(()), s))
      }
  }

  def get[U, S](implicit F: Member[State[S], U]): Free[U, S] = Free(F.inject(Get()))

  def put[U, S](state: S)(implicit F: Member[State[S], U]): Free[U, Unit] = Free(F.inject(Put(state)))

  def modify[U: State[S]#Member, S](f: S => S): Free[U, Unit] = get.flatMap(a => put(f(a)))

}

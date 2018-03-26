package kits.eff

sealed abstract class State[S] extends Product with Serializable

object State {
  def get[S: Manifest]: Eff[~[State[S]], S] = Eff(Get[S]: State[S])

  def put[S: Manifest](s: S): Eff[~[State[S]], Unit] = Eff(Put(s): State[S])

  def modify[S: Manifest](f: S => S): Eff[~[State[S]], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S: Manifest, R, A](s: S)(eff: Eff[~[State[S]] with R, A]): Eff[R, (S, A)] =
    Eff.handleRelayS[State[S], R, S, A, (S, A)](s, eff)((s, a) => Eff.Pure((s, a))) {
      case Get() => (s, k) => k(s, s)
      case Put(s) => (_, k) => k(s, ())
    }

  case class Get[S]() extends State[S]

  case class Put[S](value: S) extends State[S]
}

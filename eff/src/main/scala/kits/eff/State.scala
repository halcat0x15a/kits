package kits.eff

import scala.annotation.tailrec

sealed abstract class State[S] extends Product with Serializable

object State {
  def get[S: Manifest]: Eff[~[State[S]], S] = Eff(Get[S]: State[S])

  def put[S: Manifest](s: S): Eff[~[State[S]], Unit] = Eff(Put(s): State[S])

  def modify[S: Manifest](f: S => S): Eff[~[State[S]], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S: Manifest, R, A](s: S)(eff: Eff[~[State[S]] with R, A]): Eff[R, (S, A)] = {
    val F = manifest[State[S]]
    @tailrec
    def loop(s: S, eff: Eff[~[State[S]] with R, A]): Eff[R, (S, A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((s, a))
        case Eff.Impure(Union(F, Get()), k) => loop(s, k(s))
        case Eff.Impure(Union(F, Put(s: S)), k) => loop(s, k(()))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(s, k(a))))
      }
    def go(s: S, eff: Eff[~[State[S]] with R, A]): Eff[R, (S, A)] = loop(s, eff)
    loop(s, eff)
  }

  case class Get[S]() extends State[S]

  case class Put[S](value: S) extends State[S]
}

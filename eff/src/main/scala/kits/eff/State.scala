package kits.eff

import scala.annotation.tailrec

sealed abstract class State[S] extends Product with Serializable

object State {
  def get[S](implicit S: Manifest[S]): Eff[State[S], S] = Eff(Get(S))

  def put[S](s: S)(implicit S: Manifest[S]): Eff[State[S], Unit] = Eff(Put(S, s))

  def modify[S: Manifest](f: S => S): Eff[State[S], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S, R, A](s: S)(eff: Eff[State[S] with R, A])(implicit S: Manifest[S]): Eff[R, (S, A)] = {
    def go(s: S, eff: Eff[State[S] with R, A]): Eff[R, (S, A)] = loop(s, eff)
    @tailrec
    def loop(s: S, eff: Eff[State[S] with R, A]): Eff[R, (S, A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((s, a))
        case Eff.Impure(Get(S), k) => loop(s, k(s))
        case Eff.Impure(Put(S, s: S), k) => loop(s, k(()))
        case Eff.Impure(r, k) => Eff.Impure(r.asInstanceOf[R], Arrs((a: Any) => go(s, k(a))))
      }
    loop(s, eff)
  }

  case class Get[S](manifest: Manifest[_]) extends State[S]

  case class Put[S](manifest: Manifest[_], value: S) extends State[S]
}

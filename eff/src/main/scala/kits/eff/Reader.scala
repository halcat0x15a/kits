package kits.eff

import scala.annotation.tailrec

sealed abstract class Reader[I] extends Product with Serializable

object Reader {
  def ask[I: Manifest]: Eff[~[Reader[I]], I] = Eff(Ask[I]: Reader[I])

  def run[I: Manifest, R, A](i: I)(eff: Eff[~[Reader[I]] with R, A]): Eff[R, A] = {
    val F = manifest[Reader[I]]
    @tailrec
    def loop(eff: Eff[~[Reader[I]] with R, A]): Eff[R, A] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(a)
        case Eff.Impure(Union(F, Ask()), k) => loop(k(i))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    def go(eff: Eff[~[Reader[I]] with R, A]): Eff[R, A] = loop(eff)
    loop(eff)
  }

  def local[I: Manifest, R <: ~[Reader[I]], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run[I, R, A](r)(eff)
    }

  case class Ask[I]() extends Reader[I]
}

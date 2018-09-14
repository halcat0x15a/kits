package kits.eff

import scala.annotation.tailrec

sealed abstract class Reader[I] extends Product with Serializable

object Reader {
  def ask[I](implicit I: Manifest[I]): Eff[Reader[I], I] = Eff(Ask(I))

  def run[I, R, A](i: I)(eff: Eff[Reader[I] with R, A])(implicit I: Manifest[I]): Eff[R, A] = {
    @tailrec
    def loop(eff: Eff[Reader[I] with R, A]): Eff[R, A] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(a)
        case Eff.Impure(Union(Ask(I)), k) => loop(k(i))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    def go(eff: Eff[Reader[I] with R, A]): Eff[R, A] = loop(eff)
    loop(eff)
  }

  def local[I: Manifest, R <: Reader[I], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run[I, R, A](r)(eff)
    }

  case class Ask[I](manifest: Manifest[_]) extends Reader[I]
}

package kits.eff

import scala.annotation.tailrec

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W: Manifest](w: W): Eff[~[Writer[W]], Unit] = Eff(Tell(w): Writer[W])

  def run[W: Manifest, R, A](w: W)(eff: Eff[~[Writer[W]] with R, A])(f: (W, W) => W): Eff[R, (W, A)] = {
    val F = manifest[Writer[W]]
    @tailrec
    def loop(w: W, eff: Eff[~[Writer[W]] with R, A]): Eff[R, (W, A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((w, a))
        case Eff.Impure(Union(F, Tell(v: W)), k) => loop(f(w, v), k(()))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(w, k(a))))
      }
    def go(w: W, eff: Eff[~[Writer[W]] with R, A]): Eff[R, (W, A)] = loop(w, eff)
    loop(w, eff)
  }

  def runVector[W: Manifest, R, A](eff: Eff[~[Writer[W]] with R, A]): Eff[R, (Vector[W], A)] = {
    val F = manifest[Writer[W]]
    @tailrec
    def loop(ws: Vector[W], eff: Eff[~[Writer[W]] with R, A]): Eff[R, (Vector[W], A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((ws, a))
        case Eff.Impure(Union(F, Tell(w: W)), k) => loop(ws :+ w, k(()))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(ws, k(a))))
      }
    def go(ws: Vector[W], eff: Eff[~[Writer[W]] with R, A]): Eff[R, (Vector[W], A)] = loop(ws, eff)
    loop(Vector.empty, eff)
  }

  case class Tell[W](value: W) extends Writer[W]
}

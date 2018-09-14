package kits.eff

import scala.annotation.tailrec

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W](w: W)(implicit W: Manifest[W]): Eff[Writer[W], Unit] = Eff(Tell(W, w))

  def run[W, R, A](w: W)(eff: Eff[Writer[W] with R, A])(f: (W, W) => W)(implicit W: Manifest[W]): Eff[R, (W, A)] = {
    def go(w: W, eff: Eff[Writer[W] with R, A]): Eff[R, (W, A)] = loop(w, eff)
    @tailrec
    def loop(w: W, eff: Eff[Writer[W] with R, A]): Eff[R, (W, A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((w, a))
        case Eff.Impure(Union(Tell(W, v: W)), k) => loop(f(w, v), k(()))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(w, k(a))))
      }
    loop(w, eff)
  }

  def runVector[W, R, A](eff: Eff[Writer[W] with R, A])(implicit W: Manifest[W]): Eff[R, (Vector[W], A)] = {
    def go(ws: Vector[W], eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] = loop(ws, eff)
    @tailrec
    def loop(ws: Vector[W], eff: Eff[Writer[W] with R, A]): Eff[R, (Vector[W], A)] =
      eff match {
        case Eff.Pure(a) => Eff.Pure((ws, a))
        case Eff.Impure(Union(Tell(W, w: W)), k) => loop(ws :+ w, k(()))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(ws, k(a))))
      }
    loop(Vector.empty, eff)
  }

  case class Tell[W](manifest: Manifest[_], value: W) extends Writer[W]
}

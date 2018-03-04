package kits.eff

import scala.reflect.Manifest

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W: Manifest](w: W): Eff[~[Writer[W]], Unit] = Eff(Tell(w): Writer[W])

  def run[R, W: Manifest, A](eff: Eff[~[Writer[W]] with R, A]): Eff[R, (List[W], A)] =
    Eff.handleRelay[Writer[W], R, A, (List[W], A)](eff)(a => Eff.Pure((Nil, a))) {
      case Tell(w) => k => k(()).map { case (ws, a) => (w :: ws, a) }
    }

  case class Tell[W] private (value: W) extends Writer[W]
}

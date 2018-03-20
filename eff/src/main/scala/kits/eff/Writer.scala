package kits.eff

sealed abstract class Writer[W] extends Product with Serializable

object Writer {
  def tell[W: Manifest](w: W): Eff[~[Writer[W]], Unit] = Eff(Tell(w): Writer[W])

  def runList[R, W: Manifest, A](eff: Eff[~[Writer[W]] with R, A]): Eff[R, (List[W], A)] =
    Eff.handleRelay[Writer[W], R, A, (List[W], A)](eff)(a => Eff.Pure((Nil, a))) {
      case Tell(w) => k => k(()).map { case (ws, a) => (w :: ws, a) }
    }

  def runVector[R, W: Manifest, A](eff: Eff[~[Writer[W]] with R, A]): Eff[R, (Vector[W], A)] =
    Eff.handleRelayS[Writer[W], R, Vector[W], A, (Vector[W], A)](Vector.empty, eff)((v, a) => Eff.Pure((v, a))) {
      case Tell(w) => (v, k) => k(v :+ w, ())
    }

  case class Tell[W](value: W) extends Writer[W]
}

package kits.eff

import scala.reflect.Manifest

sealed abstract class Reader[I] extends Product with Serializable

object Reader {
  def ask[I: Manifest]: Eff[~[Reader[I]], I] = Eff(Ask[I]: Reader[I])

  def run[R, I: Manifest, A](i: I)(eff: Eff[~[Reader[I]] with R, A]): Eff[R, A] =
    Eff.handleRelay[Reader[I], R, A, A](eff)(a => Eff.Pure(a)) {
      case Ask() => k => k(i)
    }

  case class Ask[I] private () extends Reader[I]
}

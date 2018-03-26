package kits.eff

sealed abstract class Maybe extends Product with Serializable

object Maybe {
  def nothing: Eff[~[Maybe], Nothing] = Eff(Nothing: Maybe)

  def lift[A](option: Option[A]): Eff[~[Maybe], A] = option.map(Eff.Pure(_)).getOrElse(nothing)

  def run[R, A](eff: Eff[~[Maybe] with R, A]): Eff[R, Option[A]] =
    Eff.handleRelay[Maybe, R, A, Option[A]](eff)(a => Eff.Pure(Some(a))) {
      case Nothing => _ => Eff.Pure(None)
    }

  case object Nothing extends Maybe
}

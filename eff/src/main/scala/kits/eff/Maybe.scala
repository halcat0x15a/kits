package kits.eff

sealed abstract class Maybe extends Product with Serializable

object Maybe {
  def nothing: Eff[Maybe, Nothing] = Eff(Nothing)

  def lift[A](option: Option[A]): Eff[Maybe, A] =
    option match {
      case None => nothing
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Maybe with R, A]): Eff[R, Option[A]] = {
    def go(eff: Eff[Maybe with R, A]): Eff[R, Option[A]] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(Some(a))
        case Eff.Impure(Union(Nothing), _) => Eff.Pure(None)
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  case object Nothing extends Maybe
}

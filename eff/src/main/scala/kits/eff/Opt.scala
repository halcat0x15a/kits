package kits.eff

sealed abstract class Opt extends Product with Serializable

object Opt {
  def empty: Eff[Opt, Nothing] = Eff(Empty)

  def lift[A](option: Option[A]): Eff[Opt, A] =
    option match {
      case None => empty
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Opt with R, A]): Eff[R, Option[A]] = {
    def go(eff: Eff[Opt with R, A]): Eff[R, Option[A]] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(Some(a))
        case Eff.Impure(Empty, _) => Eff.Pure(None)
        case Eff.Impure(r, k) => Eff.Impure(r.asInstanceOf[R], Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  case object Empty extends Opt
}

package kits.eff

sealed abstract class Exc[E] extends Product with Serializable

object Exc {
  def fail[E](e: E)(implicit E: Manifest[E]): Eff[Exc[E], Nothing] = Eff(Fail(E, e))

  def lift[E: Manifest, A](either: Either[E, A]): Eff[Exc[E], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E, R, A](eff: Eff[Exc[E] with R, A])(implicit E: Manifest[E]): Eff[R, Either[E, A]] = {
    def go(eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(Right(a))
        case Eff.Impure(Union(Fail(E, e: E)), _) => Eff.Pure(Left(e))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  def recover[E, R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A])(implicit E: Manifest[E]): Eff[R, A] = {
    def go(eff: Eff[Exc[E] with R, A]): Eff[R, A] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(a)
        case Eff.Impure(Union(Fail(E, e: E)), _) => f(e)
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  case class Fail[E](manifest: Manifest[_], value: E) extends Exc[E]
}

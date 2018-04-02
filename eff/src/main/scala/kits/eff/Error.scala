package kits.eff

sealed abstract class Error[E] extends Product with Serializable

object Error {
  def fail[E: Manifest](e: E): Eff[~[Error[E]], Nothing] = Eff(Fail(e): Error[E])

  def lift[E: Manifest, A](either: Either[E, A]): Eff[~[Error[E]], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E: Manifest, R, A](eff: Eff[~[Error[E]] with R, A]): Eff[R, Either[E, A]] = {
    val F = manifest[Error[E]]
    def go(eff: Eff[~[Error[E]] with R, A]): Eff[R, Either[E, A]] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(Right(a))
        case Eff.Impure(Union(F, Fail(e: E)), _) => Eff.Pure(Left(e))
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  def recover[E: Manifest, R, A](eff: Eff[~[Error[E]] with R, A])(f: E => Eff[R, A]): Eff[R, A] = {
    val F = manifest[Error[E]]
    def go(eff: Eff[~[Error[E]] with R, A]): Eff[R, A] =
      eff match {
        case Eff.Pure(a) => Eff.Pure(a)
        case Eff.Impure(Union(F, Fail(e: E)), _) => f(e)
        case Eff.Impure(r: Union[R], k) => Eff.Impure(r, Arrs((a: Any) => go(k(a))))
      }
    go(eff)
  }

  case class Fail[E](e: E) extends Error[E]
}

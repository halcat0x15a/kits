package kits.eff

sealed abstract class Error[E] extends Product with Serializable

object Error {
  def fail[E: Manifest](e: E): Eff[~[Error[E]], Nothing] = Eff(Fail(e): Error[E])

  def lift[E: Manifest, A](either: Either[E, A]): Eff[~[Error[E]], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E: Manifest, R, A](eff: Eff[~[Error[E]] with R, A]): Eff[R, Either[E, A]] =
    Eff.handleRelay[Error[E], R, A, Either[E, A]](eff)(a => Eff.Pure(Right(a))) {
      case Fail(e) => _ => Eff.Pure(Left(e))
    }

  case class Fail[E](e: E) extends Error[E]
}

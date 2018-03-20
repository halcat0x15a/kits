package kits.eff

import scala.util._

sealed abstract class Error[E] extends Product with Serializable

object Error {
  def fail[E: Manifest](e: E): Eff[~[Error[E]], Nothing] = Eff(Fail(e): Error[E])

  val nothing: Eff[~[Error[Unit]], Nothing] = fail(())

  def fromEither[E: Manifest, A](either: Either[E, A]): Eff[~[Error[E]], A] = either.fold(fail(_), Eff.Pure(_))

  def fromOption[A](option: Option[A]): Eff[~[Error[Unit]], A] = option.map(Eff.Pure(_)).getOrElse(nothing)

  def fromTry[A](result: Try[A]): Eff[~[Error[Throwable]], A] = result.fold(fail(_), Eff.Pure(_))

  def runEither[R, E: Manifest, A](eff: Eff[~[Error[E]] with R, A]): Eff[R, Either[E, A]] =
    Eff.handleRelay[Error[E], R, A, Either[E, A]](eff)(a => Eff.Pure(Right(a))) {
      case Fail(e) => _ => Eff.Pure(Left(e))
    }

  def runOption[R, A](eff: Eff[~[Error[Unit]] with R, A]): Eff[R, Option[A]] =
    Eff.handleRelay[Error[Unit], R, A, Option[A]](eff)(a => Eff.Pure(Some(a))) {
      case Fail(_) => _ => Eff.Pure(None)
    }

  def runTry[R, A](eff: Eff[~[Error[Throwable]] with R, A]): Eff[R, Try[A]] =
    Eff.handleRelay[Error[Throwable], R, A, Try[A]](eff)(a => Eff.Pure(Success(a))) {
      case Fail(e) => _ => Eff.Pure(Failure(e))
    }

  def recover[R <: ~[Error[E]], E: Manifest, A](eff: Eff[R, A])(f: E => Eff[R, A]): Eff[R, A] =
    Eff.handleRelay[Error[E], R, A, A](eff)(a => Eff.Pure(a)) {
      case Fail(e) => _ => f(e)
    }

  case class Fail[E](e: E) extends Error[E]
}

package kits.free

import scala.util.{Failure, Success, Try}

sealed abstract class Error[E] {

  type Member[U] = kits.free.Member[Error[E], U]

}

object Error {

  case class Fail[E](value: E) extends Error[E]

  def run[U, E, A](free: Free[Error[E] :+: U, A]): Free[U, Either[E, A]] =
    Free.handleRelay(free, ())(
      (a, _) => Right(Right(a)),
      (fa, _, _) => fa match {
        case Fail(e) => Right(Pure(Left(e)))
      }
    )

  def fail[U, E, A](value: E)(implicit F: Member[Error[E], U]): Free[U, A] = Free(F.inject(Fail(value)))

  def recover[U: Error[E]#Member, E, A](free: Free[U, A])(handle: E => Free[U, A]): Free[U, A] =
    Free.interpose(free, ())(
      (a, _) => Right(a),
      (fa: Error[E], _, _) => fa match {
        case Fail(e) => Right(handle(e))
      }
    )

  def fromEither[U: Error[E]#Member, E, A](either: Either[E, A]): Free[U, A] = either.fold(e => fail(e), a => Pure(a))

  def fromTry[U: Error[Throwable]#Member, A](t: Try[A]): Free[U, A] =
    t match {
      case Success(a) => Pure(a)
      case Failure(e) => fail(e)
    }

  def handle[E] = new Handler {
    type Cons[U] = Error[E] :+: U
    type Result[A] = Either[E, A]
    def apply[U, A](free: Free[Error[E] :+: U, A]): Free[U, Either[E, A]] = Error.run(free)
  }

}

object Maybe {

  def run[U, A](free: Free[Maybe :+: U, A]): Free[U, Option[A]] =
    Error.run(free).map(_.fold(_ => None, Some(_)))

  def nothing[U: Maybe#Member, A]: Free[U, A] = Error.fail(())

  def fromOption[U: Maybe#Member, A](option: Option[A]): Free[U, A] = option.fold(nothing[U, A])(a => Pure(a))

  def handle[E] = new Handler {
    type Cons[U] = Maybe :+: U
    type Result[A] = Option[A]
    def apply[U, A](free: Free[Maybe :+: U, A]): Free[U, Option[A]] = Maybe.run(free)
  }

}

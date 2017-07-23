package kits.free

import scala.util.{Failure, Success, Try}

sealed abstract class Error[E] {

  type Member[U] = kits.free.Member[Error[E], U]

}

object Error {

  case class Fail[E](value: E) extends Error[E]

  def run[U, E, A](free: Free[Error[E] :+: U, A]): Free[U, Either[E, A]] =
    Free.handleRelay(free)(
      a => Right(Right(a)),
      (fa, _) => fa match {
        case Fail(e) => Right(Pure(Left(e)))
      }
    )

  def fail[U, E, A](value: E)(implicit F: Member[Error[E], U]): Free[U, A] = Free(F.inject(Fail(value)))

  def recover[U: Error[E]#Member, E, A](free: Free[U, A])(handle: E => Free[U, A]): Free[U, A] =
    Free.interpose(free)(
      a => Right(a),
      (fa: Error[E], _) => fa match {
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

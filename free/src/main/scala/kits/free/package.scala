package kits

import scala.concurrent.Future
import scala.util.Try

package object free {

  type Arrows[U, A, B] = Vector[A => Free[U, B]]

  type Maybe = Error[Unit]

  implicit class OptionAsFree[A](val option: Option[A]) extends AnyVal {
    def asFree[U: Maybe#Member]: Free[U, A] = Maybe.fromOption(option)
  }

  implicit class EitherAsFree[A, B](val either: Either[A, B]) extends AnyVal {
    def asFree[U: Error[A]#Member]: Free[U, B] = Error.fromEither(either)
  }

  implicit class TryAsFree[A](val `try`: Try[A]) extends AnyVal {
    def asFree[U: Error[Throwable]#Member]: Free[U, A] = Error.fromTry(`try`)
  }

  implicit class SeqAsFree[A](val seq: Seq[A]) extends AnyVal {
    def asFree[U: Choice#Member]: Free[U, A] = Choice.fromSeq(seq)
  }

  implicit class FutureAsFree[A](val future: Future[A]) extends AnyVal {
    def asFree[U: Lift[Future]#Member]: Free[U, A] = Lift.wrap(future)
  }

}

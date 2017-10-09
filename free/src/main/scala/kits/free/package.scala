package kits

import scala.concurrent.Future
import scala.util.Try

package object free {

  type Maybe = Error[Unit]

  implicit class OptionToFree[A](val option: Option[A]) extends AnyVal {
    def free[U: Maybe#Member]: Free[U, A] = Maybe.fromOption(option)
  }

  implicit class EitherToFree[A, B](val either: Either[A, B]) extends AnyVal {
    def free[U: Error[A]#Member]: Free[U, B] = Error.fromEither(either)
  }

  implicit class TryToFree[A](val `try`: Try[A]) extends AnyVal {
    def free[U: Error[Throwable]#Member]: Free[U, A] = Error.fromTry(`try`)
  }

  implicit class SeqToFree[A](val seq: Seq[A]) extends AnyVal {
    def free[U: Choice#Member]: Free[U, A] = Choice.fromSeq(seq)
  }

  implicit class FutureToFree[A](val future: Future[A]) extends AnyVal {
    def free[U: Lift[Future]#Member]: Free[U, A] = Lift.wrap(future)
  }

}

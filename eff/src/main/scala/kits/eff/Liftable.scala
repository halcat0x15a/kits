package kits.eff

import scala.concurrent.Future

trait Liftable[F[_]] {
  type R
  def lift[A](fa: F[A]): Eff[R, A]
}

object Liftable {
  implicit def OptionLiftable: Liftable[Option] { type R = ~[Maybe] } =
    new Liftable[Option] {
      type R = ~[Maybe]
      def lift[A](fa: Option[A]): Eff[~[Maybe], A] = Maybe.lift(fa)
    }

  implicit def EitherLiftable[E: Manifest]: Liftable[({ type F[A] = Either[E, A] })#F] { type R = ~[Error[E]] } =
    new Liftable[({ type F[A] = Either[E, A] })#F] {
      type R = ~[Error[E]]
      def lift[A](fa: Either[E, A]): Eff[~[Error[E]], A] = Error.lift(fa)
    }

  implicit def FutureLiftable: Liftable[Future] { type R = ~[Task] } =
    new Liftable[Future] {
      type R = ~[Task]
      def lift[A](fa: Future[A]): Eff[~[Task], A] = Task.lift(fa)
    }
}

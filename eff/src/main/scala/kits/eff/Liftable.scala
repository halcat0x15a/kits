package kits.eff

import scala.util.Try

trait Liftable[F[_]] {
  type R
  def lift[A](fa: F[A]): Eff[R, A]
}

object Liftable {
  implicit def OptionLiftable: Liftable[Option] { type R = ~[Error[Unit]] } =
    new Liftable[Option] {
      type R = ~[Error[Unit]]
      def lift[A](fa: Option[A]): Eff[~[Error[Unit]], A] = Error.fromOption(fa)
    }

  implicit def EitherLiftable[E: Manifest]: Liftable[({ type F[A] = Either[E, A] })#F] { type R = ~[Error[E]] } =
    new Liftable[({ type F[A] = Either[E, A] })#F] {
      type R = ~[Error[E]]
      def lift[A](fa: Either[E, A]): Eff[~[Error[E]], A] = Error.fromEither(fa)
    }

  implicit def TryLiftable: Liftable[Try] { type R = ~[Error[Throwable]] } =
    new Liftable[Try] {
      type R = ~[Error[Throwable]]
      def lift[A](fa: Try[A]): Eff[~[Error[Throwable]], A] = Error.fromTry(fa)
    }
}

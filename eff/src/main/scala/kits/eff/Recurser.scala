package kits.eff

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

trait Recurser[F, R, A, B] {
  type M[_]

  def pure(a: M[A]): Eff[R, B]

  def impure[T](fa: M[F with Fx[T]]): Either[M[T], Eff[R, B]]
}

object Recurser {
  implicit class IdentityRecurser[F, R, A, B](val recurser: Recurser[F, R, A, B] { type M[A] = A }) extends AnyVal {
    def apply(eff: Eff[F with R, A])(implicit F: TypeTag[F]): Eff[R, B] = {
      @tailrec
      def loop(eff: Eff[F with R, A]): Eff[R, B] =
        eff match {
          case Eff.Pure(a) =>
            recurser.pure(a)
          case Eff.Impure(Union(tag, fa), k) if tag.tpe <:< F.tpe =>
            recurser.impure(fa.asInstanceOf[F with Fx[Any]]) match {
              case Left(a) => loop(k(a))
              case Right(eff) => eff
            }
          case Eff.Impure(r, k) =>
            Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(k(a))))
        }
      loop(eff)
    }
  }

  implicit class StateRecurser[F, R, S, A, B](val recurser: Recurser[F, R, A, B] { type M[A] = (S, A) }) extends AnyVal {
    def apply(s: S, eff: Eff[F with R, A])(implicit F: TypeTag[F]): Eff[R, B] = {
      @tailrec
      def loop(s: S, eff: Eff[F with R, A]): Eff[R, B] =
        eff match {
          case Eff.Pure(a) =>
            recurser.pure((s, a))
          case Eff.Impure(Union(tag, fa: F with Fx[Any]), k) if tag.tpe <:< F.tpe =>
            recurser.impure((s, fa)) match {
              case Left((s, a)) => loop(s, k(a))
              case Right(eff) => eff
            }
          case Eff.Impure(r, k) =>
            Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(s, k(a))))
        }
      loop(s, eff)
    }
  }
}

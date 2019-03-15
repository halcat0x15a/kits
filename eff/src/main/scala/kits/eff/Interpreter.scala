package kits.eff

import scala.reflect.runtime.universe.TypeTag

trait Interpreter[F, R, A, B] {
  type M[_]

  def pure(a: M[A]): Eff[R, B]

  def impure[T](fa: M[F with Fx[T]])(f: M[T] => Eff[R, B]): Eff[R, B]
}

object Interpreter {
  implicit class IdentityInterpreter[F, R, A, B](val interpreter: Interpreter[F, R, A, B] { type M[A] = A }) extends AnyVal {
    final def apply(eff: Eff[F with R, A])(implicit F: TypeTag[F]): Eff[R, B] =
      eff match {
        case Eff.Pure(a) =>
          interpreter.pure(a)
        case Eff.Impure(Union(tag, fa: F with Fx[Any]), k) if tag.tpe <:< F.tpe =>
          interpreter.impure(fa)(a => apply(k(a)))
        case Eff.Impure(r, k) =>
          Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(k(a))))
      }
  }

  implicit class StateInterpreter[F, R, S, A, B](val interpreter: Interpreter[F, R, A, B] { type M[A] = (S, A) }) extends AnyVal {
    final def apply(s: S, eff: Eff[F with R, A])(implicit F: TypeTag[F]): Eff[R, B] =
      eff match {
        case Eff.Pure(a) =>
          interpreter.pure((s, a))
        case Eff.Impure(Union(tag, fa: F with Fx[Any]), k) if tag.tpe <:< F.tpe =>
          interpreter.impure((s, fa)) { case (s, a) => apply(s, k(a)) }
        case Eff.Impure(r, k) =>
          Eff.Impure(r.asInstanceOf[Union[R, Any]], Arrs.Leaf((a: Any) => apply(s, k(a))))
      }
  }
}

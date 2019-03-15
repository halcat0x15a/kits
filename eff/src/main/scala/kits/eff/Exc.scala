package kits.eff

import scala.reflect.runtime.universe.TypeTag

sealed abstract class Exc[E] extends Product with Serializable

object Exc {
  def apply[E](implicit E: TypeTag[E]): Ops[E] = new Ops(E)

  def fail[E: TypeTag](e: E): Eff[Exc[E], Nothing] = Eff(Fail(e))

  def lift[E: TypeTag, A](either: Either[E, A]): Eff[Exc[E], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E: TypeTag, R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] = {
    val handle = new Interpreter[Exc[E], R, A, Either[E, A]] {
      type M[A] = A
      def pure(a: A) = Eff.Pure(Right(a))
      def impure[T](ft: Exc[E] with Fx[T])(k: T => Eff[R, Either[E, A]]) =
        ft match {
          case Fail(e) => Eff.Pure(Left(e))
        }
    }
    handle(eff)
  }

  def recover[E, R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A])(implicit E: TypeTag[E]): Eff[R, A] = {
    val handle = new Interpreter[Exc[E], R, A, A] {
      type M[A] = A
      def pure(a: A) = Eff.Pure(a)
      def impure[T](ft: Exc[E] with Fx[T])(k: T => Eff[R, A]) =
        ft match {
          case Fail(e) => f(e)
        }
    }
    handle(eff)
  }

  case class Fail[E](value: E) extends Exc[E] with Fx[Nothing]

  class Ops[E](val manifest: TypeTag[E]) extends AnyVal {
    def run[R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] = Exc.run[E, R, A](eff)(manifest)
    def recover[R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A]): Eff[R, A] = Exc.recover[E, R, A](eff)(f)(manifest)
  }
}

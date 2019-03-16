package kits.eff

sealed abstract class Exc[E] extends Product with Serializable

object Exc {
  def fail[E: Manifest](e: E): Eff[Exc[E], Nothing] = Eff(Fail(e))

  def lift[E: Manifest, A](either: Either[E, A]): Eff[Exc[E], A] = either.fold(fail(_), Eff.Pure(_))

  def run[E: Manifest, R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] = {
    val handle = new Interpreter[Exc[E], R, A, Either[E, A]] {
      def pure(a: A) = Eff.Pure(Right(a))
      def flatMap[T](ft: Exc[E] with Fx[T])(k: T => Eff[R, Either[E, A]]) =
        ft match {
          case Fail(e) => Eff.Pure(Left(e))
        }
    }
    handle(eff)
  }

  def recover[E: Manifest, R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A]): Eff[R, A] = {
    run[E, R, A](eff).flatMap {
      case Right(a) => Eff.Pure(a)
      case Left(e) => f(e)
    }
  }

  case class Fail[E](value: E) extends Exc[E] with Fx[Nothing]

  def apply[E](implicit E: Manifest[E]): Ops[E] = new Ops(E)

  class Ops[E](val manifest: Manifest[E]) extends AnyVal {
    def run[R, A](eff: Eff[Exc[E] with R, A]): Eff[R, Either[E, A]] = Exc.run[E, R, A](eff)(manifest)
    def recover[R, A](eff: Eff[Exc[E] with R, A])(f: E => Eff[R, A]): Eff[R, A] = Exc.recover[E, R, A](eff)(f)(manifest)
  }
}

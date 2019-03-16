package kits.eff

sealed abstract class Opt extends Product with Serializable

object Opt {
  def empty: Eff[Opt, Nothing] = Eff(Empty)

  def lift[A](option: Option[A]): Eff[Opt, A] =
    option match {
      case None => empty
      case Some(a) => Eff.Pure(a)
    }

  def run[R, A](eff: Eff[Opt with R, A]): Eff[R, Option[A]] = {
    val handle = new Interpreter[Opt, R, A, Option[A]] {
      def pure(a: A) = Eff.Pure(Some(a))
      def flatMap[T](ft: Opt with Fx[T])(k: T => Eff[R, Option[A]]) =
        ft match {
          case Empty => Eff.Pure(None)
        }
    }
    handle(eff)
  }

  case object Empty extends Opt with Fx[Nothing]
}

package kits.eff

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

sealed abstract class Reader[-I] extends Product with Serializable

object Reader {
  def ask[I: TypeTag]: Eff[Reader[I], I] = Eff(Ask[I])

  def run[I: TypeTag, R, A](i: I)(eff: Eff[Reader[I] with R, A]): Eff[R, A] = {
    val handle = new Recurser[Reader[I], R, A, A] {
      type M[A] = A
      def pure(a: A) = Eff.Pure(a)
      def impure[T](ft: Reader[I] with Fx[T]) =
        ft match {
          case _: Ask[I] => Left(i)
        }
    }
    handle(eff)
  }

  def local[I: TypeTag, R <: Reader[I], A](f: I => I)(eff: Eff[R, A]): Eff[R, A] =
    ask[I].flatMap { r0 =>
      val r = f(r0)
      run[I, R, A](r)(eff)
    }

  case class Ask[I]() extends Reader[I] with Fx[I]
}

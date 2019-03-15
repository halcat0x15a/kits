package kits.eff

import scala.annotation.tailrec
import scala.reflect.runtime.universe.TypeTag

sealed abstract class State[S] extends Product with Serializable

object State {
  def apply[S](implicit S: TypeTag[S]) = new Ops(S)

  def get[S: TypeTag]: Eff[State[S], S] = Eff(Get[S])

  def put[S: TypeTag](s: S): Eff[State[S], Unit] = Eff(Put(s))

  def modify[S: TypeTag](f: S => S): Eff[State[S], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S: TypeTag, R, A](s: S)(eff: Eff[State[S] with R, A]): Eff[R, (S, A)] = {
    val handle = new Recurser[State[S], R, A, (S, A)] {
      type M[A] = (S, A)
      def pure(a: (S, A)) = Eff.Pure(a)
      def impure[T](ft: (S, State[S] with Fx[T])) =
        ft match {
          case (s, _: Get[S]) => Left((s, s))
          case (_, Put(s)) => Left((s, ()))
        }
    }
    handle(s, eff)
  }

  def transaction[S: TypeTag, R <: State[S], A](eff: Eff[R, A]): Eff[R, A] =
    for {
      s <- get
      r <- run[S, R, A](s)(eff)
      _ <- put(r._1)
    } yield r._2

  case class Get[S]() extends State[S] with Fx[S]

  case class Put[S](value: S) extends State[S] with Fx[Unit]

  class Ops[S](val manifest: TypeTag[S]) extends AnyVal {
    def transaction[R <: State[S], A](eff: Eff[R, A]): Eff[R, A] = State.transaction[S, R, A](eff)(manifest)
  }
}

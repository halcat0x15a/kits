package kits.eff

sealed abstract class State[S] extends Product with Serializable

object State {
  def get[S: Manifest]: Eff[State[S], S] = Eff(Get[S])

  def put[S: Manifest](s: S): Eff[State[S], Unit] = Eff(Put(s))

  def modify[S: Manifest](f: S => S): Eff[State[S], Unit] =
    get[S].flatMap(s => put(f(s)))

  def run[S: Manifest, R, A](s: S)(eff: Eff[State[S] with R, A]): Eff[R, (S, A)] = {
    val handle = new StateRecurser[State[S], R, S, A, (S, A)] {
      def pure(s: S, a: A) = Eff.Pure((s, a))
      def tailRec[T](s: S, ft: State[S] with Fx[T]) =
        ft match {
          case _: Get[S] => Left((s, s))
          case Put(s) => Left((s, ()))
        }
    }
    handle(s, eff)
  }

  def transaction[S: Manifest, R <: State[S], A](eff: Eff[R, A]): Eff[R, A] =
    for {
      s <- get
      r <- run[S, R, A](s)(eff)
      _ <- put(r._1)
    } yield r._2

  case class Get[S]() extends State[S] with Fx[S]

  case class Put[S](value: S) extends State[S] with Fx[Unit]

  def apply[S](implicit S: Manifest[S]) = new Ops(S)

  class Ops[S](val manifest: Manifest[S]) extends AnyVal {
    def transaction[R <: State[S], A](eff: Eff[R, A]): Eff[R, A] = State.transaction[S, R, A](eff)(manifest)
  }
}

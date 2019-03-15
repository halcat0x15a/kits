package kits.eff

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext
import scala.concurrent.Future

sealed abstract class Task extends Product with Serializable

object Task {
  def context: Eff[Task, ExecutionContext] = Eff(Context)

  def lift[A](future: => Future[A]): Eff[Task, A] = Eff(Lift(() => future))

  def async[A](a: => A): Eff[Task, A] =
    context.flatMap { implicit ec =>
      lift(Future(a))
    }

  def run[A](eff: Eff[Task, A])(implicit ec: ExecutionContext): Future[A] = {
    val handle = new Interpreter[Task, Any, A, Future[A]] {
      type M[A] = A
      def pure(a: A) = Eff.Pure(Future.successful(a))
      def impure[T](ft: Task with Fx[T])(k: T => Eff[Any, Future[A]]) =
        ft match {
          case Context => k(ec)
          case Lift(f) => Eff.Pure(f().flatMap(a => Eff.run(k(a))))
        }
    }
    Eff.run(handle(eff))
  }

  case object Context extends Task with Fx[ExecutionContext]

  case class Lift[A](future: () => Future[A]) extends Task with Fx[A]
}
